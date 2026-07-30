# Text content blocks: paragraphs (word-wrapped) and preformatted dumps.

#' A word-wrapped paragraph block
#'
#' Wraps \code{text} to the frame width using measured widths, so it never
#' overflows horizontally, reports its true multi-line height, and splits across
#' pages line-by-line.
#'
#' @param text a character string (may contain explicit newlines)
#' @param size font size in points
#' @param font face: 1 plain, 2 bold, 3 italic, 4 bold-italic
#' @param family font family ("sans"/"serif"/"mono" or a core-font name)
#' @param leading line-spacing factor
#' @param align "left", "right", "center", or "justify" (justify falls back to left in P0)
#' @param indent left indent in points
#' @param col text colour
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_para <- function(text, size = 10, font = 1L, family = "sans",
                       leading = 1.2, align = c("left", "right", "center", "justify"),
                       indent = 0, col = "black") {
  align <- match.arg(align)
  lh <- size * leading
  cache <- new.env(parent = emptyenv())

  build <- function(doc, width) {
    key <- sprintf("%.3f", width)
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)
    wrapped <- .wrap_text(doc, text, width - indent, size, font, family)
    lns <- lapply(wrapped, function(ln) {
      lw <- sp_width(doc, ln, size, font, family)
      xoff <- switch(align,
        left    = indent,
        right   = width - lw,
        center  = indent + (width - indent - lw) / 2,
        justify = indent)
      force(ln); force(xoff)
      function(doc, x, y, w) .draw_text(doc, x + xoff, y, ln, size, font, family, col)
    })
    blk <- .block_lines(lns, lh)
    cache[[key]] <- blk
    blk
  }

  list(
    measure = function(doc, width) build(doc, width)$measure(doc, width),
    draw    = function(doc, x, y, width) build(doc, width)$draw(doc, x, y, width),
    split   = function(doc, width, avail) build(doc, width)$split(doc, width, avail),
    keep    = FALSE)
}

#' A preformatted (monospace) block for capture.output() dumps
#'
#' Draws each element of \code{lines} as one physical line, and auto-shrinks the
#' font so the widest line fits the frame width (fixing the \code{options(width)}
#' wrap/overlap of dead-reckoned reports). Splits across pages line-by-line.
#'
#' @param lines a character vector, one element per line
#' @param size font size in points (upper bound; may shrink to fit width)
#' @param font face
#' @param family font family (defaults to monospace)
#' @param leading line-spacing factor
#' @param col text colour
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_pre <- function(lines, size = 9, font = 1L, family = "mono",
                      leading = 1.15, col = "black") {
  lines <- as.character(lines)
  cache <- new.env(parent = emptyenv())

  build <- function(doc, width) {
    key <- sprintf("%.3f", width)
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)
    eff <- size
    if (length(lines)) {
      maxw <- max(sp_width(doc, lines, size, font, family))
      if (is.finite(maxw) && maxw > width && maxw > 0) eff <- size * width / maxw
    }
    elh <- eff * leading
    lns <- lapply(lines, function(ln) {
      force(ln)
      function(doc, x, y, w) .draw_text(doc, x, y, ln, eff, font, family, col)
    })
    blk <- .block_lines(lns, elh)
    cache[[key]] <- blk
    blk
  }

  list(
    measure = function(doc, width) build(doc, width)$measure(doc, width),
    draw    = function(doc, x, y, width) build(doc, width)$draw(doc, x, y, width),
    split   = function(doc, width, avail) build(doc, width)$split(doc, width, avail),
    keep    = FALSE)
}
