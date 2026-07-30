# Constants and small helpers shared across simPDF.

# Paper dimensions in PDF points (1 pt = 1/72 inch).
.paper_dims <- function(paper) {
  switch(tolower(paper),
    "a4"     = c(w = 595, h = 842),
    "letter" = c(w = 612, h = 792),
    "a3"     = c(w = 842, h = 1191),
    stop("Unknown paper size: ", paper, call. = FALSE))
}

# Map a friendly family name to a PDF core font family.
.map_family <- function(f) {
  switch(as.character(f),
    mono = "Courier", sans = "Helvetica", serif = "Times",
    as.character(f))          # pass real core-font names through
}

# cex needed to draw text at `size` points on a device opened at pointsize doc$ps.
.cex <- function(doc, size) size / doc$ps

# Frame content width (points).
.fw <- function(doc) doc$frame$right - doc$frame$left

# Is the cursor at the very top of the frame (a fresh, empty page)?
.at_top <- function(doc) isTRUE(abs(doc$y - doc$frame$top) < 1e-6)

# Normalise a single block or a list of blocks into a list of blocks.
.as_block_list <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.null(x$measure)) list(x) else x
}
