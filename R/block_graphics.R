# Graphics content blocks: base-R plots drawn into a reserved region of the
# flowing page (via par(fig)), and raster images. The plot is confined to a
# band and the text canvas is restored afterwards, so diagnostics sit inline
# with measured text and pagination.

# Draw a base-R plot expression into the band [x, x+width] x [y-height, y]
# (points), then restore the full-page text canvas without clearing the page.
.draw_plot <- function(doc, x, y, width, height, pexpr, penv, mar) {
  grDevices::dev.set(doc$dev)
  # Save the full canvas par (fig/plt/usr/mar/new). The plot's own plot.new()
  # runs with new = TRUE (below), so it OVERLAYS the current page rather than
  # advancing it; restoring par afterwards re-establishes the canvas coordinate
  # system WITHOUT an extra plot.new() (which would desync device paging).
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = FALSE)
  graphics::par(fig = c(x / doc$W, (x + width) / doc$W,
                        (y - height) / doc$H, y / doc$H), mar = mar, new = TRUE)
  eval(pexpr, envir = penv)
  invisible()
}

#' A block that draws a base-R plot into a reserved region
#'
#' Evaluates a plotting expression (e.g. \code{plot(...)}, \code{hist(...)},
#' \code{qqnorm(...)}) inside a band of the given height on the flowing page.
#' The plot is confined to the band and the text canvas is restored afterwards,
#' so plots sit inline with text and participate in pagination (a plot that does
#' not fit the remaining space moves to the next page). The expression is
#' captured unevaluated and run at draw time in the caller's environment.
#'
#' @param expr a plotting expression (not evaluated until drawn)
#' @param height band height in points
#' @param width band width in points (default: full frame width)
#' @param mar plot margins in lines, \code{par("mar")} style
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_plot <- function(expr, height, width = NULL, mar = c(4, 4, 2, 1)) {
  pexpr <- substitute(expr)
  penv  <- parent.frame()
  list(
    measure = function(doc, w) height,
    draw    = function(doc, x, y, w) {
      ww <- if (is.null(width)) w else width
      .draw_plot(doc, x, y, ww, height, pexpr, penv, mar)
    },
    keep = FALSE, split = NULL)
}

# Coerce assorted image inputs to a raster object.
.as_raster <- function(img) {
  if (inherits(img, "raster")) return(img)
  if (is.array(img) || is.matrix(img)) return(grDevices::as.raster(img))
  if (is.character(img) && length(img) == 1L && file.exists(img)) {
    ext <- tolower(tools::file_ext(img))
    if (ext == "png") {
      if (!requireNamespace("png", quietly = TRUE))
        stop("block_image: package 'png' is needed to read PNG files", call. = FALSE)
      return(grDevices::as.raster(png::readPNG(img)))
    }
    if (ext %in% c("jpg", "jpeg")) {
      if (!requireNamespace("jpeg", quietly = TRUE))
        stop("block_image: package 'jpeg' is needed to read JPEG files", call. = FALSE)
      return(grDevices::as.raster(jpeg::readJPEG(img)))
    }
    stop("block_image: unsupported image type '", ext, "'", call. = FALSE)
  }
  stop("block_image: 'img' must be a raster, array/matrix, or PNG/JPEG path", call. = FALSE)
}

#' Draw a full-page multi-panel figure (outside the flow engine)
#'
#' For per-subject / per-parameter diagnostic pages that are a whole page of
#' base-R graphics (a \code{par(mfrow)} grid), rather than a block within the
#' text flow. Starts a fresh page, sets \code{mfrow}/\code{oma}/\code{mar}, runs
#' the plotting expression, and counts one page. The expression should draw
#' exactly \code{prod(mfrow)} panels (pad with blank \code{plot.new()} if fewer)
#' so each call maps to one page.
#'
#' @param doc a document handle
#' @param expr base-R plotting expression (not evaluated until drawn)
#' @param mfrow panel grid, e.g. \code{c(2, 3)}
#' @param oma,mar outer/inner margins in lines
#' @return The document handle \code{doc}, invisibly (the page counter is
#'   advanced in place). Called for its side effect of drawing one full page
#'   of panels.
#' @export
sp_figure_page <- function(doc, expr, mfrow = c(1, 1), oma = c(0, 0, 2, 0),
                           mar = c(4, 4, 2, 1)) {
  pexpr <- substitute(expr); penv <- parent.frame()
  grDevices::dev.set(doc$dev)
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(mfrow = mfrow, oma = oma, mar = mar, new = FALSE)
  eval(pexpr, envir = penv)
  doc$page_no <- doc$page_no + 1L
  invisible(doc)
}

#' A block that embeds a raster image
#'
#' Places an image (a raster object, a numeric array/matrix, or a PNG/JPEG file
#' path) into a band of the given height on the flowing page.
#'
#' @param img a raster, array/matrix, or path to a PNG/JPEG file
#' @param height band height in points
#' @param width band width in points (default: full frame width)
#' @param interpolate passed to \code{\link[graphics]{rasterImage}}
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; the constructor itself draws nothing.
#' @export
block_image <- function(img, height, width = NULL, interpolate = TRUE) {
  raster <- .as_raster(img)
  list(
    measure = function(doc, w) height,
    draw    = function(doc, x, y, w) {
      ww <- if (is.null(width)) w else width
      grDevices::dev.set(doc$dev)
      graphics::rasterImage(raster, x, y - height, x + ww, y, interpolate = interpolate)
      invisible()
    },
    keep = FALSE, split = NULL)
}
