## block_plot / block_image: page-advance sync and inline placement.

phys_pages <- function(f) {
  r <- readBin(f, "raw", file.info(f)$size); r <- r[r != as.raw(0)]
  m <- gregexpr("/Type */Page[^s]", rawToChar(r), useBytes = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

## Regression: block_plot must not desync the device page count. Three tall
## plots force a page break; doc$page_no must equal the physical page count.
out <- tempfile(fileext = ".pdf")
doc <- sp_new(out, paper = "letter", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
flow_run(doc, list(
  block_plot(plot(1:10), height = 250),
  block_plot(plot(10:1), height = 250),
  block_plot(hist(rnorm(50), main = ""), height = 250),
  block_para("PAGE 2 TEXT", size = 14, font = 2)))
np <- doc$page_no
sp_close(doc)
expect_equal(np, 2L)
expect_equal(phys_pages(out), 2L)

## Text after a plot lands below the plot band (cursor advanced by height).
out2 <- tempfile(fileext = ".pdf")
doc <- sp_new(out2, paper = "letter", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
sp_trace(doc, TRUE)
flow_run(doc, list(block_para("ABOVE", 12, font = 2),
                   block_plot(plot(1:10), height = 200),
                   block_para("BELOW", 12, font = 2)))
tx <- unlist(doc$trace$texts); B <- do.call(rbind, doc$trace$boxes)
expect_true(B[which(tx == "ABOVE"), "y1"] - B[which(tx == "BELOW"), "y1"] >= 200)
sp_close(doc)

## block_image renders from an RGB array without error.
out3 <- tempfile(fileext = ".pdf")
doc <- sp_new(out3, paper = "letter", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
expect_silent(flow_run(doc, list(block_image(array(runif(20 * 30 * 3), c(20, 30, 3)), height = 120))))
sp_close(doc)
