## Flow engine: zero overlap, auto-pagination, block_pre never overflows width.

lorem <- "The quick brown fox jumps over the lazy dog while measuring text width precisely in points on the PDF graphics device."

## --- multi-page synthetic report: zero overlapping glyphs ---
out <- tempfile(fileext = ".pdf")
doc <- sp_new(out, paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 50, bottom = 40, left = 40, right = doc$W - 40)
sp_trace(doc, TRUE)

blocks <- list(
  block_para("simPDF flow test", size = 16, font = 2),
  block_rule(),
  block_keep(list(block_para("Heading", size = 12, font = 2),
                  block_para(paste(rep(lorem, 2), collapse = " "), size = 10))))
for (i in 1:55)
  blocks <- c(blocks, list(block_para(sprintf("Para %02d. %s", i,
             paste(rep(lorem, (i %% 3) + 1), collapse = " ")), 10)))
wide <- vapply(1:80, function(k)
  paste(sprintf("V%02d=% .4f", 1:12, (k + 1:12) / 7), collapse = "  "), character(1))
blocks <- c(blocks, list(block_para("Wide dump:", 12, font = 2), block_pre(wide, size = 9)))

flow_run(doc, blocks, footer = block_para("CONFIDENTIAL", 8, align = "center"))
ov  <- sp_overlaps(doc)
npg <- doc$page_no
nbx <- length(doc$trace$boxes)
sp_close(doc)

expect_true(npg >= 3L)                  # auto-paginated
expect_true(nbx >= 200L)                # a substantial (200+ line) document
expect_equal(nrow(ov), 0L)              # ZERO overlaps
expect_true(file.exists(out) && file.info(out)$size > 1000)
expect_equal(rawToChar(readBin(out, "raw", 4L)), "%PDF")

## --- block_pre auto-shrinks so nothing exceeds the frame width ---
out2 <- tempfile(fileext = ".pdf")
doc <- sp_new(out2, paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 40, bottom = 40, left = 40, right = doc$W - 40)
sp_trace(doc, TRUE)
verylong <- paste(rep("WWWWWWWWWWWWWWWWWWWW", 12), collapse = " ")
flow_run(doc, list(block_pre(rep(verylong, 5), size = 12)))
B <- do.call(rbind, doc$trace$boxes)
fr <- doc$frame$right
sp_close(doc)
expect_true(all(B[, "x1"] <= fr + 0.5))

## --- exact fit within float error: no NULL tail, no spurious page break ---
out3 <- tempfile(fileext = ".pdf")
doc <- sp_new(out3, paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 40, bottom = 40, left = 40, right = doc$W - 40)
sp_page(doc)
b <- block_para("the last line of the page", size = 10)
h <- b$measure(doc, doc$frame$right - doc$frame$left)
doc$y <- doc$frame$bottom + h - 1e-12     # avail == h up to float error
expect_silent(flow_add(doc, b))
expect_equal(doc$page_no, 1L)             # drawn here, not pushed to a new page
sp_close(doc)

## --- same edge for a table (header + rows exactly fill the frame) ---
out4 <- tempfile(fileext = ".pdf")
doc <- sp_new(out4, paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 40, bottom = 40, left = 40, right = doc$W - 40)
sp_page(doc)
tb <- block_table(data.frame(a = 1:8, b = letters[1:8]))
h <- tb$measure(doc, doc$frame$right - doc$frame$left)
doc$y <- doc$frame$bottom + h - 1e-12
expect_silent(flow_add(doc, tb))
expect_equal(doc$page_no, 1L)
sp_close(doc)
