# Shared Huff functions. Station observations enter fitting, never attraction.
# ---- th03-origin-demand ----
origin_weights <- function(origins, residential_share, office_share = 0, employment_share = 0) {
  activity <- origins$activity / sum(origins$activity)
  if (office_share > 0) activity <- (1 - office_share) * activity +
    office_share * origins$office_area / sum(origins$office_area)
  if (employment_share > 0) activity <- (1 - employment_share) * activity +
    employment_share * origins$employment / sum(origins$employment)
  residential_share * origins$residents / sum(origins$residents) +
    (1 - residential_share) * activity
}

# ---- th03-choice-probabilities ----
station_features <- function(destinations, bus_radius = 500) {
  mrt <- if (!"codes" %in% names(destinations)) rep(0, nrow(destinations)) else
    as.numeric(grepl("(^|/)(NS|EW|CG|NE|CC|CE|DT|TE|JS|JE|JW|CR)[0-9]", destinations$codes))
  direct <- paste0("direct_bus_", bus_radius)
  competition <- if (direct %in% names(destinations)) -log1p(destinations[[direct]]) else rep(0, nrow(destinations))
  cbind(rail = log(destinations$lines),
        bus = log1p(destinations[[paste0("bus_", bus_radius)]]), mrt = mrt, competition = competition)
}

station_attraction <- function(parameters, destinations, bus_radius = 500) {
  effects <- c(rail = 0, bus = 0, mrt = 0, competition = 0)
  effects[intersect(names(parameters), names(effects))] <- parameters[intersect(names(parameters), names(effects))]
  exp(as.numeric(station_features(destinations, bus_radius) %*% effects))
}

choice_probabilities <- function(parameters, distances, destinations,
                                 bus_radius = 500, distance_floor = 0.1) {
  attraction <- log(station_attraction(parameters, destinations, bus_radius))
  utility <- sweep(-parameters["decay"] * log(pmax(distances, distance_floor)),
                   2, attraction, "+")
  # The fitted parameter bounds keep these weights finite for the study distances.
  weights <- exp(utility)
  weights / rowSums(weights)
}

# ---- th03-fit-model ----
fit_huff <- function(observed, distances, origins, destinations, model = 3,
                     training = seq_along(observed), bus_radius = 500, distance_floor = 0.1,
                     residential_only = FALSE) {
  parameter_names <- c("decay", "residential", "rail", "bus", "mrt")[seq_len(c(1, 2, 4, 5, 4, 5, 5, 5, 5, 5)[model])]
  if (model %in% c(7, 9)) parameter_names <- c(parameter_names, "office")
  if (model %in% c(8, 10)) parameter_names <- c(parameter_names, "employment")
  if (model %in% c(9, 10)) parameter_names <- c(parameter_names, "competition")
  if (residential_only) parameter_names <- setdiff(parameter_names, c("residential", "office", "employment"))
  unpack <- function(x) {
    parameters <- c(decay = 2, residential = 1, rail = 0, bus = 0, mrt = 0, office = 0,
                    employment = 0, competition = 0)
    parameters[parameter_names] <- x
    parameters
  }
  log_distance <- log(pmax(distances, distance_floor))
  features <- station_features(destinations, bus_radius)
  residential <- origins$residents / sum(origins$residents)
  activity <- origins$activity / sum(origins$activity)
  office <- if (model %in% c(7, 9)) origins$office_area / sum(origins$office_area) else rep(0, nrow(origins))
  employment <- if (model %in% c(8, 10)) origins$employment / sum(origins$employment) else rep(0, nrow(origins))
  actual <- observed[training]
  previous <- NULL
  result <- NULL
  # The analytic derivative avoids repeated large-matrix calculations during fitting.
  evaluate <- function(x) {
    if (identical(x, previous)) return(result)
    parameters <- unpack(x)
    weights <- exp(sweep(-parameters["decay"] * log_distance, 2,
                         as.numeric(features %*% parameters[c("rail", "bus", "mrt", "competition")]), "+"))
    probabilities <- weights / rowSums(weights)
    mix <- (1 - parameters["office"] - parameters["employment"]) * activity +
      parameters["office"] * office + parameters["employment"] * employment
    demand <- parameters["residential"] * residential + (1 - parameters["residential"]) * mix
    shares <- as.numeric(crossprod(demand, probabilities))
    # Held-out counts contribute neither to the scale nor to the loss.
    scale <- sum(actual) / sum(shares[training])
    expected <- pmax(scale * shares[training], 1e-12)
    terms <- ifelse(actual > 0, actual * log(pmax(actual, 1e-12) / expected), 0)
    distance_term <- probabilities * log_distance
    derivatives <- cbind(
      decay = -colSums(demand * distance_term) +
        as.numeric(crossprod(demand * rowSums(distance_term), probabilities)),
      residential = as.numeric(crossprod(residential - mix, probabilities)),
      features * shares - crossprod(probabilities, demand * (probabilities %*% features)),
      office = as.numeric(crossprod((1 - parameters["residential"]) * (office - activity), probabilities)),
      employment = as.numeric(crossprod((1 - parameters["residential"]) * (employment - activity), probabilities)))
    result <<- list(loss = 2 * sum(terms - actual + expected),
      gradient = as.numeric(2 * crossprod(scale - actual / pmax(shares[training], 1e-12),
                                          derivatives[training, parameter_names, drop = FALSE])),
      predicted = scale * shares)
    previous <<- x
    result
  }
  starts <- expand.grid(decay = c(1, 2, 4), residential = if (model > 1) c(0.25, 0.75) else 1,
                        rail = 0.4, bus = 0.4, mrt = 0.4, office = 0.2, employment = 0.2, competition = 0.2)
  if (model >= 7) {
    starts$rail <- 1.5
    starts$bus <- 0.1
    starts$mrt <- 2.3
  }
  fits <- lapply(seq_len(nrow(starts)), function(i) {
    optim(unlist(starts[i, parameter_names, drop = FALSE]), function(x) evaluate(x)$loss,
          gr = function(x) evaluate(x)$gradient, method = "L-BFGS-B",
          lower = c(decay = 0.25, residential = 0, rail = 0, bus = 0, mrt = 0, office = 0, employment = 0, competition = 0)[parameter_names],
          upper = c(decay = 6, residential = 1, rail = 3, bus = 3, mrt = 4, office = 1, employment = 1, competition = 3)[parameter_names],
          control = list(maxit = 300))
  })
  successful <- which(vapply(fits, function(x) x$convergence == 0 && is.finite(x$value), logical(1)))
  if (!length(successful)) stop("Huff calibration did not converge; inspect the inputs before reporting estimates.")
  best <- fits[[successful[which.min(vapply(fits[successful], `[[`, numeric(1), "value"))]]]
  list(parameters = unpack(best$par), predicted = evaluate(best$par)$predicted, loss = best$value,
       model = model, bus_radius = bus_radius, distance_floor = distance_floor)
}

# ---- th03-mci-probabilities ----
# The matrix calculation above is used in optimisation; verify its output against MCI.
mci_probabilities <- function(fit, interactions, origins, destinations) {
  parameters <- fit$parameters
  attraction <- station_attraction(parameters, destinations, fit$bus_radius)
  pairs <- interactions %>%
    mutate(attraction = .env$attraction[match(station_id, destinations$station_id)],
           distance = pmax(distance_km, fit$distance_floor))
  pairs <- as.data.frame(pairs)
  # MCI repeatedly grows an internal table. Separate origins give identical
  # probabilities while keeping that table small for the finer demand locations.
  by_origin <- split(pairs, factor(pairs$origin_id, levels = origins$origin_id))
  do.call(rbind, lapply(by_origin, function(x) {
    shares <- MCI::huff.shares(x, "origin_id", "station_id", "attraction", "distance",
                              gamma = 1, lambda = -unname(parameters["decay"]), check_df = FALSE)
    shares$p_ij[match(destinations$station_id, shares$station_id)]
  }))
}

# ---- th03-error-measures ----
error_measures <- function(observed, predicted) {
  tibble(MAE = mean(abs(observed - predicted)),
         RMSE = sqrt(mean((observed - predicted)^2)),
         WAPE = sum(abs(observed - predicted)) / sum(observed))
}

# ---- th03-passenger-import ----
read_entries <- function(month, raw_dir, code_crosswalk) {
  path <- file.path(raw_dir, paste0("origin_destination_train_", month, ".csv"))
  archive <- sub("\\.csv$", ".zip", path)
  if (!file.exists(path) && !file.exists(archive)) return(NULL)
  input <- if (file.exists(path)) path else unz(archive, basename(path), open = "rb")
  if (inherits(input, "connection")) on.exit(close(input))
  trips <- readr::read_csv(input, show_col_types = FALSE,
                          col_types = readr::cols(ORIGIN_PT_CODE = "c", DESTINATION_PT_CODE = "c"))
  required <- c("YEAR_MONTH", "DAY_TYPE", "TIME_PER_HOUR", "ORIGIN_PT_CODE", "DESTINATION_PT_CODE", "TOTAL_TRIPS")
  stopifnot(all(required %in% names(trips)), !anyNA(trips[required]),
            all(gsub("-", "", trips$YEAR_MONTH) == month), all(trips$TOTAL_TRIPS >= 0),
            all(trips$TIME_PER_HOUR %in% 0:23),
            !anyDuplicated(trips[setdiff(required, "TOTAL_TRIPS")]))
  weekdays <- trips %>% filter(DAY_TYPE == "WEEKDAY")
  boarding_codes <- unique(weekdays$ORIGIN_PT_CODE)
  lookup <- lapply(boarding_codes, function(code) {
    pieces <- strsplit(code, "[-/]")[[1]]
    # LTA's 2026 records use CC33/CC34; the station workbook uses CE2/CE1.
    pieces <- dplyr::recode(pieces, CC33 = "CE2", CC34 = "CE1")
    match <- code_crosswalk %>% filter(network == "Existing", .data$code %in% pieces)
    if (!all(pieces %in% match$code) || n_distinct(match$station_id) != 1) {
      stop("Unmatched or ambiguous boarding station code: ", code)
    }
    tibble(ORIGIN_PT_CODE = code, station_id = match$station_id[1])
  }) %>% bind_rows()
  calendar <- seq(as.Date(paste0(substr(month, 1, 4), "-", substr(month, 5, 6), "-01")),
                   by = "day", length.out = 31)
  # MOM: National Day observed on 10 August. No September public holidays.
  n_weekdays <- sum(format(calendar, "%Y%m") == month &
                      as.integer(format(calendar, "%u")) <= 5 & calendar != as.Date("2026-08-10"))
  result <- weekdays %>% left_join(lookup, by = "ORIGIN_PT_CODE") %>%
    group_by(station_id) %>% summarise(monthly_entries = sum(TOTAL_TRIPS), .groups = "drop") %>%
    mutate(entries = monthly_entries / n_weekdays, month = month, weekdays = n_weekdays)
  stopifnot(sum(result$monthly_entries) == sum(weekdays$TOTAL_TRIPS))
  result
}
