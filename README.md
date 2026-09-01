# Le Truong Chinh — ISSS626 GIS field notebook

A Quarto website for documenting coursework in **ISSS626: Geospatial Analytics and Applications** under the guidance of **Prof Kam Tin Seong**.

The starter intentionally contains no hands-on or in-class exercise content. It provides a distinct visual system, an empty fieldwork archive, an about page, and Netlify deployment configuration.

## Preview locally

```powershell
quarto preview
```

## Render

```powershell
quarto render
```

The rendered site is written to `_site/`.

## Deploy to Netlify

Render the site, commit both the source files and the generated `_site/` folder, then push the repository. Import that repository into Netlify; the included `netlify.toml` points Netlify to `_site/`, so no build command is required.

Alternatively, after signing in to Netlify from Quarto:

```powershell
quarto publish netlify
```

This local-render workflow is deliberate: future R, Python, or Julia analysis is executed on your computer, while Netlify only serves the finished static site.

## Add future work

Create new `.qmd` files for your own analyses, then link them from `work.qmd` or convert a collection into a Quarto listing when several entries exist.
