# simPDF

Fast, base-R, multi-page **PDF reports** on the graphics device.

`simPDF` lays out reports on R's built-in `pdf()` / `cairo_pdf()` device with a
**measured flowing layout**: every block reports its real width and height, the
cursor advances by that measured height, and pages break automatically. Text,
tables and matrices therefore **never overlap**, however many rows or parameters
they contain — the failure mode of reports built from hand-computed coordinates.

It is a fast, dependency-light alternative to `.Rmd`/`knitr`/`LaTeX` for
fixed-format reports: no external toolchain, one write pass, base R only.

Interactive fillable forms (AcroForm CRFs) are out of scope and handled by the
sibling package **pdfCRF**.

## Installation

```r
# from CRAN (once available)
install.packages("simPDF")

# from source
install.packages("simPDF_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Quick start

```r
library(simPDF)

doc <- sp_new("report.pdf", paper = "letter")
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)

flow_run(doc, list(
  block_para("Summary", size = 16, font = 2),
  block_rule(),
  block_table(head(mtcars)),                       # measured columns, repeats header, splits pages
  block_para("Diagnostics", size = 12, font = 2),
  block_plot(plot(mpg ~ wt, mtcars), height = 220) # a plot, inline with the text
), footer = block_para("CONFIDENTIAL", size = 8, align = "center"))

sp_close(doc)
```

See `vignette("simPDF")` for the full guide.

## Features

* **Flow engine** — `sp_new()`, `frame_set()`, `flow_run()`; blocks:
  `block_para` (wrapped/aligned text), `block_pre` (`capture.output()` dumps),
  `block_table` (auto column widths, repeating header, page-splitting),
  `block_matrix` (wide matrices split into column groups), `block_plot`
  (inline plots), `sp_figure_page()` (full-page multi-panel figures),
  `block_image`, `block_keep`, `block_spacer`, `block_rule`, running
  headers/footers.
* **No overlap** — AFM-exact measurement (`sp_width`/`sp_height`) drives every
  placement; `sp_trace()` / `sp_overlaps()` let you assert zero overlap in tests.
* **Signatures** — `block_authorship()` and `block_signature()` draw a
  prepared-by block and approval lines; `sp_add_sig_fields()` adds interactive
  Adobe Acrobat signature fields (`/Sig`) to the finished PDF via a pure base-R
  incremental update.
* **Model flow diagrams** — `block_flow_diagram()` renders a tree of nodes as
  measured boxes, scaled to fit the page.

## Related packages

* **nmw** uses `simPDF` for its NONMEM diagnostic reports.
* **pdfCRF** (sibling) builds interactive clinical CRFs by raw PDF emission.

## License

GPL-3. Author/maintainer: Kyun-Seop Bae <k@acr.kr>.
