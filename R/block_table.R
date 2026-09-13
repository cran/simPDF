# Table and matrix content blocks with measured column widths, automatic
# row-boundary page splitting (header repeated), and wide-matrix column
# grouping. These replace capture.output() dumps and hand-placed column loops.

# Turn a data.frame/matrix into header + character body + per-column alignment.
# Row names (when meaningful) become a left-aligned leading label column.
.tabularize <- function(x, header = TRUE) {
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(x)
  aligns <- vapply(x, function(col) if (is.numeric(col)) "r" else "l", character(1))
  cols <- lapply(x, function(col) format(col, trim = TRUE))
  # Pass ncol explicitly so a 0-row input keeps its columns (and headers),
  # instead of collapsing to a 0x0 matrix.
  body <- if (length(cols))
    matrix(unlist(cols, use.names = FALSE), nrow = n, ncol = length(cols)) else
    matrix(character(0), nrow = n, ncol = 0)
  hdr <- if (header) colnames(x) else NULL
  rn <- rownames(x)
  has_rn <- !is.null(rn) && !identical(as.character(rn), as.character(seq_len(n)))
  if (has_rn) {
    body <- cbind(as.character(rn), body)
    aligns <- c("l", aligns)
    if (!is.null(hdr)) hdr <- c("", hdr)
  }
  list(hdr = hdr, body = body, align = aligns, n_label = as.integer(has_rn))
}

# Draw one table row (a character vector of cells) with its top at y.
# Padding lives INSIDE each column (colw already = cell + 2*pad), so the row
# spans exactly x .. x + sum(colw).
.draw_row <- function(doc, x, y, cells, colw, aligns, size, family, pad, font = 1L, col = "black") {
  cx <- x
  for (j in seq_along(cells)) {
    w <- sp_width(doc, cells[j], size, font, family)
    xoff <- switch(aligns[j],
      l = pad, r = colw[j] - pad - w, c = (colw[j] - w) / 2, pad)
    .draw_text(doc, cx + xoff, y, cells[j], size, font, family, col)
    cx <- cx + colw[j]
  }
  invisible()
}

# A self-contained, splittable table block with fixed column widths / size.
# `rows` is a list of character vectors (one per body row). The header is
# repeated on every page fragment produced by split().
.table_core <- function(hdr, rows, colw, aligns, size, family, leading, pad,
                        font = 1L, rule = TRUE) {
  rowh   <- size * leading
  ruleh  <- if (rule && !is.null(hdr)) 3 else 0
  headh  <- if (!is.null(hdr)) rowh + ruleh else 0
  totw   <- sum(colw)

  draw_frag <- function(doc, x, y) {
    yy <- y
    if (!is.null(hdr)) {
      .draw_row(doc, x, yy, hdr, colw, aligns, size, family, pad, font)
      yy <- yy - rowh
      if (ruleh > 0) {
        grDevices::dev.set(doc$dev)
        graphics::lines(c(x, x + totw), c(yy + ruleh - 1, yy + ruleh - 1), xpd = NA)
        yy <- yy - ruleh
      }
    }
    for (r in rows) { .draw_row(doc, x, yy, r, colw, aligns, size, family, pad, font); yy <- yy - rowh }
    invisible()
  }

  self <- list(
    keep    = FALSE,
    measure = function(doc, width) headh + length(rows) * rowh,
    draw    = function(doc, x, y, width) draw_frag(doc, x, y),
    split   = function(doc, width, avail) {
      usable <- avail - headh
      nfit <- floor((usable + 1e-9) / rowh)
      if (nfit <= 0L) return(list(head = NULL, tail = self))
      if (nfit >= length(rows)) return(list(head = self, tail = NULL))
      list(
        head = .table_core(hdr, rows[seq_len(nfit)], colw, aligns, size, family, leading, pad, font, rule),
        tail = .table_core(hdr, rows[(nfit + 1L):length(rows)], colw, aligns, size, family, leading, pad, font, rule))
    }
  )
  self
}

# Natural (unconstrained) width of each column = widest cell (incl header) + 2*pad.
.natural_widths <- function(doc, hdr, body, size, family, font, pad) {
  ncol <- ncol(body)
  vapply(seq_len(ncol), function(j) {
    cells <- c(if (!is.null(hdr)) hdr[j], body[, j])
    w <- sp_width(doc, cells, size, family = family, font = font)
    (if (length(w)) max(w) else 0) + 2 * pad   # finite width for 0-row / empty columns
  }, numeric(1))
}

# Build a fitted table over a given set of columns, shrinking the font uniformly
# if the natural widths exceed `frameW`.
.fit_table <- function(doc, hdr, body, aligns, cols, frameW, size, family,
                       leading, pad, font, rule) {
  b   <- body[, cols, drop = FALSE]
  h   <- if (!is.null(hdr)) hdr[cols] else NULL
  al  <- aligns[cols]
  nat <- .natural_widths(doc, h, b, size, family, font, pad)
  tot <- sum(nat)
  if (tot <= frameW || tot <= 0) {
    eff <- size; colw <- nat; pad_eff <- pad
  } else {
    f <- frameW / tot                       # shrink font, widths AND padding together
    eff <- size * f; colw <- nat * f; pad_eff <- pad * f
  }
  rows <- if (nrow(b)) lapply(seq_len(nrow(b)), function(i) b[i, ]) else list()
  .table_core(h, rows, colw, al, eff, family, leading, pad_eff, font, rule)
}

#' A table block with measured column widths and automatic page splitting
#'
#' Renders a data.frame or matrix as a table: column widths are measured from
#' the content, numeric columns are right-aligned, the header repeats on every
#' page, and the body splits at row boundaries when it does not fit. If the
#' natural width exceeds the frame, the font is shrunk uniformly to fit (use
#' \code{\link{block_matrix}} to instead split a wide matrix into column groups).
#'
#' @param x a data.frame or matrix
#' @param size font size in points (upper bound; may shrink to fit width)
#' @param family font family
#' @param font face
#' @param header show the column-name header row
#' @param leading line-spacing factor
#' @param pad horizontal cell padding in points
#' @param rule draw a rule under the header
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_table <- function(x, size = 9, family = "sans", font = 1L, header = TRUE,
                        leading = 1.3, pad = 4, rule = TRUE) {
  tab <- .tabularize(x, header = header)
  cache <- new.env(parent = emptyenv())
  build <- function(doc, width) {
    key <- sprintf("%.3f", width); hit <- cache[[key]]; if (!is.null(hit)) return(hit)
    blk <- .fit_table(doc, tab$hdr, tab$body, tab$align, seq_len(ncol(tab$body)),
                      width, size, family, leading, pad, font, rule)
    cache[[key]] <- blk; blk
  }
  list(
    measure = function(doc, width) build(doc, width)$measure(doc, width),
    draw    = function(doc, x, y, width) build(doc, width)$draw(doc, x, y, width),
    split   = function(doc, width, avail) build(doc, width)$split(doc, width, avail),
    keep    = FALSE)
}

# A vertical sequence of sub-blocks that can split between and within members.
.block_seq <- function(subs, gap = 0) {
  subs <- Filter(Negate(is.null), subs)
  self <- list(
    keep    = FALSE,
    measure = function(doc, width) {
      if (!length(subs)) return(0)
      sum(vapply(subs, function(b) b$measure(doc, width), numeric(1))) +
        gap * (length(subs) - 1L)
    },
    draw = function(doc, x, y, width) {
      yy <- y
      for (b in subs) { h <- b$measure(doc, width); b$draw(doc, x, yy, width); yy <- yy - h - gap }
      invisible()
    },
    split = function(doc, width, avail) {
      head <- list(); acc <- 0; i <- 1L; n <- length(subs)
      while (i <= n) {
        h <- subs[[i]]$measure(doc, width)
        if (acc + h <= avail) { head[[length(head) + 1L]] <- subs[[i]]; acc <- acc + h + gap; i <- i + 1L }
        else break
      }
      if (i > n) return(list(head = self, tail = NULL))
      rem <- avail - acc
      parts <- if (!is.null(subs[[i]]$split)) subs[[i]]$split(doc, width, rem)
               else list(head = NULL, tail = subs[[i]])
      if (!is.null(parts$head)) head[[length(head) + 1L]] <- parts$head
      tailsubs <- c(if (!is.null(parts$tail)) list(parts$tail),
                    if (i < n) subs[(i + 1L):n] else NULL)
      if (!length(head)) return(list(head = NULL, tail = self))
      list(head = .block_seq(head, gap),
           tail = if (length(tailsubs)) .block_seq(tailsubs, gap) else NULL)
    }
  )
  self
}

#' A wide-matrix block that groups columns to fit the page width
#'
#' Renders a matrix; if it is wider than the frame, the data columns are split
#' into groups that each fit (row labels repeated in every group), and the
#' groups are stacked vertically. This replaces hand-placed column loops such as
#' \code{PrinTxt(row, i * 8, ...)} that overflow the page for many columns.
#'
#' @param m a matrix (or data.frame)
#' @param size font size in points
#' @param family font family (monospace by default for aligned numbers)
#' @param font face
#' @param leading line-spacing factor
#' @param pad horizontal cell padding in points
#' @param gap vertical gap between column groups (points)
#' @param max_cols_per_group optional cap on data columns per group
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_matrix <- function(m, size = 9, family = "mono", font = 1L, leading = 1.3,
                         pad = 4, gap = size * 0.8, max_cols_per_group = NULL) {
  tab <- .tabularize(m, header = TRUE)
  cache <- new.env(parent = emptyenv())
  build <- function(doc, width) {
    key <- sprintf("%.3f", width); hit <- cache[[key]]; if (!is.null(hit)) return(hit)
    nl <- tab$n_label
    ndat <- ncol(tab$body) - nl
    if (ndat <= 0L) {
      blk <- .fit_table(doc, tab$hdr, tab$body, tab$align, seq_len(ncol(tab$body)),
                        width, size, family, leading, pad, font, TRUE)
      cache[[key]] <- blk; return(blk)
    }
    nat <- .natural_widths(doc, tab$hdr, tab$body, size, family, font, pad)
    labw <- if (nl > 0L) sum(nat[seq_len(nl)]) else 0
    datidx <- (nl + 1L):ncol(tab$body)
    # Greedy grouping of data columns so label + group fits `width`.
    groups <- list(); cur <- integer(0); accw <- labw
    for (j in datidx) {
      over_w <- accw + nat[j] > width && length(cur) > 0
      over_n <- !is.null(max_cols_per_group) && length(cur) >= max_cols_per_group
      if (over_w || over_n) { groups[[length(groups) + 1L]] <- cur; cur <- integer(0); accw <- labw }
      cur <- c(cur, j); accw <- accw + nat[j]
    }
    if (length(cur)) groups[[length(groups) + 1L]] <- cur
    labels <- if (nl > 0L) seq_len(nl) else integer(0)
    grp_blocks <- lapply(groups, function(g)
      .fit_table(doc, tab$hdr, tab$body, tab$align, c(labels, g),
                 width, size, family, leading, pad, font, TRUE))
    blk <- .block_seq(grp_blocks, gap = gap)
    cache[[key]] <- blk; blk
  }
  list(
    measure = function(doc, width) build(doc, width)$measure(doc, width),
    draw    = function(doc, x, y, width) build(doc, width)$draw(doc, x, y, width),
    split   = function(doc, width, avail) build(doc, width)$split(doc, width, avail),
    keep    = FALSE)
}
