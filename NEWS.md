# simPDF NEWS

## simPDF 0.1.1

Documentation-only release for the CRAN resubmission (no code changes).

- Added the missing `\value` sections to all exported functions' help pages
  (CRAN review): `flow_run()`, `flow_add()`, `remaining()`,
  `running_header()`/`running_footer()`, `block_keep()`, `block_spacer()`,
  `block_rule()`, `frame_set()`, `sp_page()`, `sp_close()`, `sp_trace()`.
- Expanded the existing `\value` sections to describe the structure (class)
  and meaning of every return value, e.g. what a "block" is (a list with
  `measure`/`draw`/`keep`/`split` components consumed by the flow engine)
  and that document-handle returns are environments updated in place.

## simPDF 0.1.0

### P0 — Core canvas, measurement, and flow engine (done)

First working slice of the device-based report engine (see `PLAN-simPDF.md`).

- **Document / canvas**: `sp_new()` opens a `pdf()`/`cairo_pdf()` device with a
  point coordinate system; `sp_page()`, `sp_close()`, `frame_set()`.
- **Measurement**: `sp_width()` / `sp_height()`. AFM-exact for the core fonts and
  linear in size for *fractional* sizes too (measured at a reference size and
  scaled, working around the `pdf()` device's integer font-size rounding in
  `strwidth`).
- **Flow engine**: `flow_run()` / `flow_add()` place measured blocks, advancing
  the cursor by real height and paginating automatically; `block_keep()`
  (keep-together), `block_spacer()`, `block_rule()`, `remaining()`,
  `running_header()` / `running_footer()`.
- **Content blocks**: `block_para()` (word-wrapped, aligned) and `block_pre()`
  (monospace `capture.output()` dumps, auto-shrunk to fit the frame width).
- **QA**: `sp_trace()` / `sp_overlaps()` record and detect overlapping text
  bounding boxes. Tests in `inst/tinytest/` assert a 200+ line synthetic report
  renders with zero overlap across multiple pages.

This removes the dead-reckoned-coordinate overlap of the `nmw` diagnostic
reports (see `inst/examples/nmw_params.R`).

### P1 - Tables and matrices (done)

- **`block_table()`**: renders a data.frame/matrix with column widths measured
  from the content, numeric columns right-aligned, a header that repeats on
  every page, a rule under the header, and automatic row-boundary page
  splitting. If the natural width exceeds the frame, the font shrinks uniformly
  to fit. Row names become a leading label column.
- **`block_matrix()`**: renders a wide matrix by splitting its data columns into
  groups that each fit the page width (row labels repeated in every group),
  stacked vertically -- replacing hand-placed column loops such as
  `PrinTxt(row, i * 8, ...)` that overflow for many columns.
- Tracer now records the drawn text of each box, so tests assert header-repeat
  directly. `inst/tinytest/test_table.R` covers a 200-row split table (header
  repeated once per page) and a 20-column matrix split into groups, both with
  zero overlap.

#### P1 fixes (from an adversarial review pass)

- `block_table`/`block_matrix` shrink-to-fit now scales the cell **padding** by
  the same factor as the font and column widths. Previously only font and widths
  were scaled, so at strong shrink (`tot > 2 * frameW`) the last column's text
  spilled past the frame's right edge (and a right-aligned first column past the
  left). Regression-tested.
- A 0-row data.frame/matrix now renders its column **headers** instead of a
  blank band: `.tabularize` preserves the column count when there are no rows,
  and `.natural_widths` guards the empty-column `max()` so `header = FALSE`
  0-row input no longer warns. Regression-tested.

### P2 - Inline graphics (done)

- **`block_plot(expr, height)`**: evaluates a base-R plotting expression (scatter,
  histogram, QQ, ...) inside a reserved band of the flowing page via `par(fig)`,
  then restores the text canvas. Plots sit inline with measured text, are kept
  with their headings via `block_keep`, and participate in pagination. The
  expression is captured unevaluated and run at draw time in the caller's
  environment.
- **`block_image(img, height)`**: embeds a raster, array/matrix, or PNG/JPEG file
  (`png`/`jpeg` in Suggests) via `rasterImage`.
- Fixed a page-desync bug found in visual verification: the canvas is now
  restored with `par(op)` alone (no extra `plot.new()`), so a page break after a
  `block_plot` advances the physical PDF page in lock-step with `doc$page_no`.
  Regression-tested by comparing `doc$page_no` to the physical `/Type /Page`
  count. See `inst/examples/nmw_diagnostics.R`.

### Authorship, signatures, and diagrams (done)

For downstream packages that build reports on simPDF:

- **`block_authorship()`** and **`block_signature()`**: a "Prepared by" metadata
  block and visible approval/signature lines (role caption + dated line). The
  signing rectangles are recorded on `doc$sig`.
- **`sp_add_sig_fields()`**: a finishing pass that inserts interactive Adobe
  Acrobat digital-signature fields (AcroForm `/Sig`) into the written PDF via a
  pure base-R incremental update (no external tools) -- generalised from
  `NonCompart::addSigFieldNCA`, and driven by the `doc$sig` rectangles so the
  clickable fields land exactly on the visible lines. The result is
  one-click-signable in the free Acrobat Reader and passes `qpdf --check`.
- **`block_flow_diagram()`**: a model-development flow diagram where each node is
  a box measured to its text, laid out as a tidy tree (children centred under
  parents) and scaled uniformly to fit the frame -- so more models shrink the
  diagram instead of cluttering it (unlike the dead-reckoned legacy `Outline`).

### nmw report migration (drafted + validated)

All 8 `nmw` diagnostic reports now have simPDF implementations (S2-Parameters
plus the remaining seven: S1-OFV, S3-Predictions, S4-Residuals, S5-EBE,
SA-Input, SB-Output, SC-IndiPK). Each keeps the parsing/statistics verbatim and
replaces the dead-reckoned `pdf()`+`PrinTxt`/`split.screen` layout with a
`blocks` list + `flow_run()` (text/tables) and `sp_figure_page()` (per-subject /
per-eta plot pages). Validated on the bundled `2006` example (all 8 generate)
and across real runs (97/98 report generations on 14 models); the
S3-Predictions "zero-length labels" crash on subjects with no observations is
fixed (was 11/18, now 14/14). These functions live with the `nmw` package
(which gains `simPDF` in Imports); simPDF gained `sp_figure_page()` to host the
full-page multi-panel diagnostic pages.

### Next

- Land the migrated `report_*` functions in `nmw` (replace bodies, drop the old
  `PrepPDF`/`PrinTxt`/`AddPage`/`PrinMTxt`/`ClosePDF` from `pdf_report.R`).
- Optional pagination for very large flow diagrams (currently scale-to-fit).
