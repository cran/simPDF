# Model-development flow diagram as a measured, auto-scaled block.
#
# The legacy Outline() draws every node's text at dead-reckoned offsets on one
# un-scaled plot, so many models overlap. Here each node becomes a box sized to
# its measured text; a tidy-tree layout places children under parents; and the
# whole diagram is scaled to fit the frame, so it never clutters.

#' A model-development flow-diagram block
#'
#' Renders a tree of nodes (e.g. NONMEM model runs) as boxes connected
#' parent-to-child. Each box is sized to its measured multi-line label; the tree
#' is laid out tidily (children centred under parents) and scaled uniformly to
#' fit the reserved band, so adding more models shrinks the diagram instead of
#' overlapping it.
#'
#' @param nodes a data.frame with columns \code{id}, \code{parent} (an id, or
#'   \code{NA}/"" for a root) and \code{label} (a string; use "\\n" for
#'   multi-line node text)
#' @param height band height in points
#' @param size maximum node font size in points (shrinks with the scale)
#' @param box_pad padding inside boxes in points
#' @param vgap,hgap vertical/horizontal gaps between nodes in points
#' @param connect draw parent-child connectors
#' @param box_col,line_col box border and connector colours
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_flow_diagram <- function(nodes, height = 500, size = 8, box_pad = 4,
                               vgap = 34, hgap = 14, connect = TRUE,
                               box_col = "black", line_col = "grey55") {
  ids  <- as.character(nodes$id)
  par_ <- as.character(nodes$parent); par_[is.na(par_)] <- ""
  labs <- strsplit(as.character(nodes$label), "\n", fixed = TRUE)
  n <- length(ids)
  kids   <- lapply(ids, function(id) which(par_ == id))
  isroot <- !(par_ %in% ids)

  depth <- rep(NA_integer_, n); depth[isroot] <- 0L
  repeat {
    prog <- FALSE
    for (i in seq_len(n)) if (is.na(depth[i])) {
      p <- match(par_[i], ids)
      if (!is.na(p) && !is.na(depth[p])) { depth[i] <- depth[p] + 1L; prog <- TRUE }
    }
    if (!prog) break
  }
  depth[is.na(depth)] <- 0L

  xslot <- rep(NA_real_, n); ctr <- new.env(parent = emptyenv()); ctr$v <- 0
  assign_x <- function(i) {
    ch <- kids[[i]]
    if (!length(ch)) { xslot[i] <<- ctr$v; ctr$v <- ctr$v + 1; return(xslot[i]) }
    cx <- vapply(ch, assign_x, numeric(1)); xslot[i] <<- mean(cx); xslot[i]
  }
  for (r in which(isroot)) assign_x(r)
  if (anyNA(xslot)) xslot[is.na(xslot)] <- seq_len(sum(is.na(xslot))) - 1  # orphans

  list(
    keep = FALSE, split = NULL,
    measure = function(doc, width) height,
    draw = function(doc, x, y, width) {
      grDevices::dev.set(doc$dev)
      lh <- size * 1.15
      bw <- vapply(seq_len(n), function(i)
        max(sp_width(doc, labs[[i]], size, 1L, doc$family)) + 2 * box_pad, numeric(1))
      bh <- vapply(seq_len(n), function(i) length(labs[[i]]) * lh + 2 * box_pad, numeric(1))
      slotw <- max(bw) + hgap; rowh <- max(bh) + vgap
      cx  <- xslot * slotw + slotw / 2
      cyt <- depth * rowh
      totalW <- (max(xslot) + 1) * slotw
      totalH <- (max(depth) + 1) * rowh - vgap
      s <- min(1, width / totalW, height / totalH)
      px <- function(X) x + X * s
      py <- function(Y) y - Y * s
      es <- size * s

      if (connect) for (i in seq_len(n)) {
        p <- match(par_[i], ids)
        if (!is.na(p))
          graphics::lines(c(px(cx[p]), px(cx[i])),
                          c(py(cyt[p] + bh[p]), py(cyt[i])), col = line_col, xpd = NA)
      }
      for (i in seq_len(n)) {
        w <- bw[i] * s; h <- bh[i] * s
        bx0 <- px(cx[i]) - w / 2; by1 <- py(cyt[i]); by0 <- by1 - h
        graphics::rect(bx0, by0, bx0 + w, by1, border = box_col, xpd = NA)
        for (k in seq_along(labs[[i]]))
          .draw_text(doc, bx0 + box_pad * s, by1 - box_pad * s - (k - 1) * (es * 1.15),
                     labs[[i]][k], es, 1L, doc$family)
      }
      invisible()
    })
}
