## block_table: page-split with header repeat, zero overlap.
set.seed(11)
big <- data.frame(ID = 1:200, TIME = round(runif(200, 0, 24), 2),
                  DV = round(rlnorm(200), 3), PRED = round(rlnorm(200), 3),
                  WT = round(runif(200, 50, 90), 1))
out <- tempfile(fileext = ".pdf")
doc <- sp_new(out, paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
sp_trace(doc, TRUE)
flow_run(doc, list(block_para("Input Data", 13, font = 2), block_table(big, size = 9)))
np <- doc$page_no
ov <- sp_overlaps(doc)
tx <- unlist(doc$trace$texts)
sp_close(doc)

expect_true(np >= 3L)                          # paginated
expect_equal(nrow(ov), 0L)                     # zero overlap
expect_equal(sum(tx == "PRED"), np)            # header repeated once per page

## block_matrix: wide matrix splits into >1 column group, stays in frame.
Mw <- matrix(round(rnorm(8 * 20, 0, 0.4), 4), 8, 20,
             dimnames = list(paste0("R", 1:8), paste0("C", 1:20)))
out2 <- tempfile(fileext = ".pdf")
doc <- sp_new(out2, paper = "letter", family = "Courier", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
sp_trace(doc, TRUE)
flow_run(doc, list(block_matrix(Mw, size = 9)))
B <- do.call(rbind, doc$trace$boxes)
fr <- doc$frame$right
tx2 <- unlist(doc$trace$texts)
ov2 <- sp_overlaps(doc)
sp_close(doc)

expect_true(all(B[, "x1"] <= fr + 0.5))        # no group exceeds frame width
expect_equal(nrow(ov2), 0L)                    # zero overlap
expect_true(sum(tx2 == "R1") >= 2L)            # row label repeated -> multiple groups

## Narrow table fits one page with a header.
out3 <- tempfile(fileext = ".pdf")
doc <- sp_new(out3, paper = "letter", size = 10)
sp_trace(doc, TRUE)
flow_run(doc, list(block_table(
  data.frame(Param = c("CL", "V", "KA"), Est = c(3.21, 30.5, 1.12)), size = 10)))
expect_equal(doc$page_no, 1L)
expect_equal(nrow(sp_overlaps(doc)), 0L)
sp_close(doc)

## Regression: strong shrink (tot > 2*frameW) must scale padding too, so no
## cell text spills past the frame left/right edges.
df <- as.data.frame(matrix(sprintf("val_%03d", 1:60), 3, 20), stringsAsFactors = FALSE)
names(df) <- sprintf("HeaderCol_%02d", 1:20)
df[[1]] <- c(123456789.5, 987654321.2, 555555555.9)   # numeric -> right-aligned first col
out4 <- tempfile(fileext = ".pdf")
doc <- sp_new(out4, paper = "A4", margin = 25, size = 9)
sp_trace(doc, TRUE)
flow_run(doc, list(block_table(df, size = 9, pad = 4)))
B <- do.call(rbind, doc$trace$boxes)
expect_true(all(B[, "x1"] <= doc$frame$right + 0.01))
expect_true(all(B[, "x0"] >= doc$frame$left - 0.01))
sp_close(doc)

## Regression: a 0-row table still renders its column headers (not a blank band).
out5 <- tempfile(fileext = ".pdf")
doc <- sp_new(out5, paper = "letter", size = 10)
sp_trace(doc, TRUE)
flow_run(doc, list(block_table(data.frame(Alpha = 1:3, Beta = 4:6)[0, ])))
expect_true(all(c("Alpha", "Beta") %in% unlist(doc$trace$texts)))
sp_close(doc)

## Regression: 0-row with header = FALSE must not warn (empty-max -> -Inf width).
out6 <- tempfile(fileext = ".pdf")
doc <- sp_new(out6, paper = "letter", size = 10)
expect_silent(flow_run(doc, list(block_table(data.frame(A = 1:3, B = 4:6)[0, ], header = FALSE))))
sp_close(doc)
