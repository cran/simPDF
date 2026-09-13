# simPDF

Multi-page **PDF reports** in a fraction of a second, in base R.

## Why: speed

The usual way to produce a PDF report from R is `.Rmd` → `knitr` → `pandoc` →
LaTeX. That chain starts three external programs, writes the document to disk
three times, and lets TeX re-typeset the whole thing in several global passes.
For a fixed-format report — the same layout, new numbers, run after run — you
pay that cost every single time.

`simPDF` skips the chain. It draws straight onto R's built-in `pdf()` /
`cairo_pdf()` device in **one pass**, so there is no toolchain to start and no
intermediate format to convert.

The same report — 40 paragraphs, a 200-row table, one scatter plot — written
both ways (R 4.6.1, Windows 11, median of three runs):

| | elapsed |
|---|---|
| `simPDF` | **0.21 s** |
| `.Rmd` → `pandoc` → pdfLaTeX | 4.42 s |

**About 20× faster**, and nothing outside R has to be installed: no pandoc, no
TeX distribution, no `.Rmd` file. A batch of a hundred diagnostic reports takes
seconds instead of minutes, which is what makes it practical to regenerate them
on every model run.

## And: no overlapping text

`simPDF` lays reports out with a **measured flowing layout**: every block
reports its real width and height, the cursor advances by that measured height,
and pages break automatically. Text, tables and matrices therefore **never
overlap**, however many rows or parameters they contain — the failure mode of
reports built from hand-computed coordinates.

Interactive fillable forms (AcroForm CRFs) are out of scope: `simPDF` is for
reports and figures.

## Installation

```r
install.packages("simPDF")

# development version
# install.packages("remotes")
remotes::install_github("ksbae/simPDF")
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

## License

GPL-3. Author/maintainer: Kyun-Seop Bae <k@acr.kr>.
