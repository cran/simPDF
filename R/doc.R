# Document handle, page lifecycle, frame, and running header/footer.
#
# The document is a mutable reference object (an environment), like a
# connection: functions mutate it in place. No global state, no `<<-`.

#' Open a new simPDF document
#'
#' Opens a graphics-device PDF and returns a mutable document handle. Text is
#' laid out in PDF points with the origin at the bottom-left of the page. A
#' default content frame is set inside \code{margin} on all sides.
#'
#' @param file output PDF path
#' @param paper "A4", "letter", or "A3"
#' @param margin page margin in points
#' @param family default font family
#' @param size default font size in points
#' @param pointsize device pointsize (cex base); leave at default
#' @param cairo use \code{cairo_pdf} (font embedding, Unicode/CJK) instead of \code{pdf}
#' @param onefile single multi-page file (TRUE) vs one file per page
#' @return A document handle: an \code{environment} holding the open graphics
#'   device, page geometry, content frame, cursor position and running
#'   header/footer state. It is mutable and is updated in place by the other
#'   simPDF functions; pass it to them and finish with \code{\link{sp_close}}.
#' @export
sp_new <- function(file, paper = "A4", margin = 25, family = "Helvetica",
                   size = 10, pointsize = 12, cairo = FALSE, onefile = TRUE) {
  dims <- .paper_dims(paper)
  doc <- new.env(parent = emptyenv())
  doc$file    <- file
  doc$paper   <- paper
  doc$W       <- unname(dims[["w"]])
  doc$H       <- unname(dims[["h"]])
  doc$margin  <- margin
  doc$family  <- family
  doc$size    <- size
  doc$ps      <- pointsize
  doc$page_no <- 0L
  doc$header  <- NULL
  doc$footer  <- NULL
  doc$trace   <- NULL

  if (cairo) {
    grDevices::cairo_pdf(file, width = doc$W / 72, height = doc$H / 72,
                         pointsize = pointsize, family = family, onefile = onefile)
  } else {
    grDevices::pdf(file, width = doc$W / 72, height = doc$H / 72,
                   pointsize = pointsize, family = family, onefile = onefile,
                   title = "simPDF report")
  }
  doc$dev <- grDevices::dev.cur()

  frame_set(doc, top = doc$H - margin, bottom = margin,
            left = margin, right = doc$W - margin)
  doc
}

#' Set the content frame (and reset the cursor to its top)
#'
#' @param doc a document handle
#' @param top,bottom,left,right frame edges in points
#' @return The document handle \code{doc}, invisibly (an environment; the
#'   frame and the cursor are updated in place). Called for this side effect.
#' @export
frame_set <- function(doc, top, bottom, left, right) {
  doc$frame <- list(top = top, bottom = bottom, left = left, right = right)
  doc$y <- top
  invisible(doc)
}

#' Start a new page
#'
#' Emits a new PDF page, resets the cursor to the frame top, and re-stamps the
#' running header/footer.
#'
#' @param doc a document handle
#' @param redraw_running redraw running header/footer on the new page
#' @return The document handle \code{doc}, invisibly (the page counter and
#'   cursor are updated in place). Called for its side effect of emitting a
#'   new PDF page.
#' @export
sp_page <- function(doc, redraw_running = TRUE) {
  grDevices::dev.set(doc$dev)
  graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, doc$W), ylim = c(0, doc$H),
                        xaxs = "i", yaxs = "i")
  doc$page_no <- doc$page_no + 1L
  doc$y <- doc$frame$top
  if (redraw_running) .draw_running(doc)
  invisible(doc)
}

#' Close the document (flush the PDF)
#'
#' @param doc a document handle
#' @return No return value (\code{invisible(NULL)}); called for its side
#'   effect of closing the graphics device, which writes and finalises the
#'   PDF file.
#' @export
sp_close <- function(doc) {
  grDevices::dev.off(doc$dev)
  invisible(NULL)
}

# Draw running header (in the top margin) and footer (in the bottom margin).
.draw_running <- function(doc) {
  fw <- .fw(doc)
  if (!is.null(doc$header)) {
    yy <- doc$H - 10
    for (b in doc$header) {
      h <- b$measure(doc, fw); b$draw(doc, doc$frame$left, yy, fw); yy <- yy - h
    }
  }
  if (!is.null(doc$footer)) {
    yy <- doc$frame$bottom - 4
    for (b in doc$footer) {
      h <- b$measure(doc, fw); b$draw(doc, doc$frame$left, yy, fw); yy <- yy - h
    }
  }
  invisible()
}

#' Turn bounding-box tracing on for no-overlap checks
#'
#' @param doc a document handle
#' @param on logical
#' @return The document handle \code{doc}, invisibly (the tracing state is
#'   toggled in place). Called for its side effect; the boxes recorded while
#'   tracing are inspected with \code{\link{sp_overlaps}}.
#' @export
sp_trace <- function(doc, on = TRUE) {
  doc$trace <- if (on) new.env(parent = emptyenv()) else NULL
  if (on) { doc$trace$boxes <- list(); doc$trace$texts <- list() }
  invisible(doc)
}

#' Return overlapping text bounding-box pairs recorded while tracing
#'
#' @param doc a document handle
#' @param eps overlap tolerance in points
#' @return A \code{data.frame} with one row per overlapping pair of recorded
#'   text bounding boxes and columns \code{i}, \code{j} (indices of the two
#'   boxes in the trace) and \code{page} (the page they overlap on). It has
#'   zero rows when nothing overlaps.
#' @export
sp_overlaps <- function(doc, eps = 0.1) {
  if (is.null(doc$trace) || !length(doc$trace$boxes))
    return(data.frame(i = integer(), j = integer(), page = integer()))
  B <- do.call(rbind, doc$trace$boxes)
  n <- nrow(B)
  out <- list()
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    if (B[i, "page"] != B[j, "page"]) next
    xov <- min(B[i, "x1"], B[j, "x1"]) - max(B[i, "x0"], B[j, "x0"])
    yov <- min(B[i, "y1"], B[j, "y1"]) - max(B[i, "y0"], B[j, "y0"])
    if (xov > eps && yov > eps)
      out[[length(out) + 1L]] <- data.frame(i = i, j = j, page = B[i, "page"])
  }
  if (!length(out)) data.frame(i = integer(), j = integer(), page = integer())
  else do.call(rbind, out)
}
