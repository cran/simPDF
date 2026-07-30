# The flow engine: measured block placement with automatic pagination.
#
# A block is a list(measure, draw, keep, split):
#   measure(doc, width) -> height in points
#   draw(doc, x, y, width)  draws with the block's TOP at y
#   keep  logical, kept together (atomic)
#   split(doc, width, avail) -> list(head, tail) or NULL if not splittable;
#         head fits in `avail`, tail is the remainder (either may be NULL)

# Build a splittable block from a list of one-line draw closures of equal pitch.
# Each element is function(doc, x, y, width) drawing one line with top at y.
.block_lines <- function(lns, lineheight, keep = FALSE) {
  n <- length(lns)
  self <- list(
    measure = function(doc, width) n * lineheight,
    draw = function(doc, x, y, width) {
      for (i in seq_len(n)) lns[[i]](doc, x, y - (i - 1L) * lineheight, width)
      invisible()
    },
    keep = keep,
    split = function(doc, width, avail) {
      nfit <- floor((avail + 1e-9) / lineheight)
      if (nfit <= 0L)  return(list(head = NULL, tail = self))
      if (nfit >= n)   return(list(head = self, tail = NULL))
      list(head = .block_lines(lns[seq_len(nfit)], lineheight, keep),
           tail = .block_lines(lns[(nfit + 1L):n], lineheight, keep))
    }
  )
  self
}

# Place one block at the cursor, paginating (and splitting) as needed.
.place_block <- function(doc, b, gap = 0) {
  if (is.null(b)) return(invisible(doc))
  if (doc$page_no == 0L) sp_page(doc)
  repeat {
    fw    <- .fw(doc)
    h     <- b$measure(doc, fw)
    avail <- doc$y - doc$frame$bottom

    if (h <= avail) {                              # fits: draw and advance
      b$draw(doc, doc$frame$left, doc$y, fw)
      doc$y <- doc$y - h - gap
      break
    }

    if (!is.null(b$split)) {                       # try to fill current page
      parts <- b$split(doc, fw, avail)
      if (!is.null(parts$head)) {
        parts$head$draw(doc, doc$frame$left, doc$y, fw)
        sp_page(doc)
        b <- parts$tail
        next
      }
    }

    if (.at_top(doc)) {                            # atomic block taller than a page
      warning("simPDF: block is taller than the frame; content overflows the page")
      b$draw(doc, doc$frame$left, doc$y, fw)
      doc$y <- doc$y - h - gap
      break
    }

    sp_page(doc)                                   # not at top: break to a fresh page
  }
  invisible(doc)
}

#' Run a sequence of blocks through the flow engine
#'
#' Measures each block, draws it if it fits, otherwise starts a new page
#' (splitting splittable blocks across the break). \code{NULL} entries are
#' dropped, so conditional blocks (\code{if (cond) block_*()}) are fine.
#'
#' @param doc a document handle
#' @param blocks a list of blocks
#' @param header,footer running header/footer block(s) for every page
#' @param gap inter-block vertical gap in points
#' @return The document handle \code{doc}, invisibly. The handle is an
#'   environment updated in place (pages are added and the cursor advances);
#'   the function is called mainly for its side effect of drawing the blocks
#'   into the PDF.
#' @export
flow_run <- function(doc, blocks, header = NULL, footer = NULL, gap = 4) {
  if (!is.null(header)) doc$header <- .as_block_list(header)
  if (!is.null(footer)) doc$footer <- .as_block_list(footer)
  blocks <- Filter(Negate(is.null), blocks)
  if (doc$page_no == 0L) sp_page(doc) else .draw_running(doc)
  for (b in blocks) .place_block(doc, b, gap)
  invisible(doc)
}

#' Add a single block imperatively
#'
#' @param doc a document handle
#' @param block a block, or NULL (no-op)
#' @param gap inter-block gap in points
#' @return The document handle \code{doc}, invisibly (an environment, updated
#'   in place). Called for its side effect of drawing \code{block} into the
#'   PDF.
#' @export
flow_add <- function(doc, block, gap = 4) .place_block(doc, block, gap)

#' Vertical space remaining in the current frame (points)
#'
#' @param doc a document handle
#' @return A single numeric value: the vertical space in points still
#'   available between the cursor and the bottom of the content frame.
#' @export
remaining <- function(doc) doc$y - doc$frame$bottom

#' Set the running header / footer
#'
#' @param doc a document handle
#' @param blocks a block or list of blocks
#' @return The document handle \code{doc}, invisibly (the running blocks are
#'   stored on the handle in place). Called for its side effect: the header
#'   (top margin) or footer (bottom margin) is re-drawn on every subsequent
#'   page.
#' @name running
#' @export
running_header <- function(doc, blocks) {
  doc$header <- .as_block_list(blocks); invisible(doc)
}

#' @rdname running
#' @export
running_footer <- function(doc, blocks) {
  doc$footer <- .as_block_list(blocks); invisible(doc)
}

#' Keep a group of blocks together on one page (atomic)
#'
#' @param blocks a list of blocks
#' @param gap inter-block gap inside the group
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place the whole group on one page; the constructor itself draws
#'   nothing.
#' @export
block_keep <- function(blocks, gap = 0) {
  blocks <- Filter(Negate(is.null), blocks)
  list(
    keep  = TRUE,
    split = NULL,
    measure = function(doc, width) {
      hs <- vapply(blocks, function(b) b$measure(doc, width), numeric(1))
      sum(hs) + gap * max(0L, length(blocks) - 1L)
    },
    draw = function(doc, x, y, width) {
      yy <- y
      for (b in blocks) {
        h <- b$measure(doc, width); b$draw(doc, x, yy, width); yy <- yy - h - gap
      }
      invisible()
    }
  )
}

#' A vertical spacer block
#'
#' @param height height in points
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. When placed by \code{\link{flow_run}} or
#'   \code{\link{flow_add}} it consumes \code{height} points of vertical
#'   space without drawing anything.
#' @export
block_spacer <- function(height) {
  list(measure = function(doc, width) height,
       draw = function(doc, x, y, width) invisible(),
       keep = FALSE, split = NULL)
}

#' A horizontal rule spanning the frame width
#'
#' @param color line colour
#' @param pad vertical padding above/below the rule (points)
#' @param lwd line width
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to draw the rule at the cursor; the constructor itself draws nothing.
#' @export
block_rule <- function(color = "black", pad = 2, lwd = 1) {
  h <- 2 * pad + 1
  list(
    measure = function(doc, width) h,
    draw = function(doc, x, y, width) {
      grDevices::dev.set(doc$dev)
      yy <- y - pad
      graphics::lines(c(x, x + width), c(yy, yy), col = color, lwd = lwd, xpd = NA)
      invisible()
    },
    keep = FALSE, split = NULL)
}
