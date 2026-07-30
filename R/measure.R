# Text measurement (AFM-exact for core fonts) and low-level text drawing.

#' Measure text width in points
#'
#' Returns the rendered width of \code{text} in PDF points, using the open
#' device's font metrics. Measured in inches and scaled by 72, so it does not
#' require an active plot page and never has a side effect on the document.
#' AFM-exact for the 14 core PDF fonts (identical to what the viewer draws).
#'
#' @param doc a simPDF document handle from \code{\link{sp_new}}
#' @param text character vector to measure
#' @param size font size in points
#' @param font integer face: 1 plain, 2 bold, 3 italic, 4 bold-italic
#' @param family font family ("sans"/"serif"/"mono" or a core-font name)
#' @return A numeric vector of the same length as \code{text}: the rendered
#'   width of each element in PDF points (1/72 inch).
#' @export
sp_width <- function(doc, text, size = doc$size, font = 1L, family = doc$family) {
  grDevices::dev.set(doc$dev)
  op <- graphics::par(family = .map_family(family))
  on.exit(graphics::par(op))
  # The pdf() device rounds the font size to an integer for strwidth's AFM
  # lookup, so measuring at the target size is wrong for fractional sizes.
  # Measure at a large integer reference size (exact) and scale linearly --
  # core-font widths are exactly proportional to size.
  REF <- 1000
  graphics::strwidth(text, units = "inches", cex = REF / doc$ps, font = font) *
    72 * (size / REF)
}

#' Line height (vertical advance) in points
#'
#' @param doc a simPDF document handle
#' @param size font size in points
#' @param lines number of lines
#' @param leading line-spacing factor (line pitch = size * leading)
#' @return A single numeric value: the vertical space in points that
#'   \code{lines} lines occupy at the given \code{size} and \code{leading}
#'   (\code{lines * size * leading}).
#' @export
sp_height <- function(doc, size = doc$size, lines = 1, leading = 1.2) {
  lines * size * leading
}

# Draw one line of text with its TOP-LEFT corner at (x, y). Records a bounding
# box when tracing is on (used by the no-overlap tests).
.draw_text <- function(doc, x, y, text, size, font = 1L, family = doc$family,
                       col = "black") {
  grDevices::dev.set(doc$dev)
  op <- graphics::par(family = .map_family(family))
  on.exit(graphics::par(op))
  graphics::text(x, y, labels = text, adj = c(0, 1), cex = .cex(doc, size),
                 font = font, col = col, xpd = NA)
  if (!is.null(doc$trace)) {
    w <- sp_width(doc, text, size, font, family)
    doc$trace$boxes[[length(doc$trace$boxes) + 1L]] <-
      c(page = doc$page_no, x0 = x, x1 = x + w, y0 = y - size, y1 = y)
    doc$trace$texts[[length(doc$trace$texts) + 1L]] <- text
  }
  invisible()
}

# Greedy word-wrap of a (possibly multi-line) string to a width in points.
# Splits on explicit newlines first, then wraps each paragraph by words.
.wrap_text <- function(doc, text, width, size, font, family) {
  paras <- strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  if (length(paras) == 0L) paras <- ""
  out <- character(0)
  for (p in paras) {
    words <- strsplit(p, "[ \t]+")[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0L) { out <- c(out, ""); next }
    cur <- words[1L]
    for (w in words[-1L]) {
      cand <- paste(cur, w)
      if (sp_width(doc, cand, size, font, family) <= width) {
        cur <- cand
      } else {
        out <- c(out, cur); cur <- w
      }
    }
    out <- c(out, cur)
  }
  out
}
