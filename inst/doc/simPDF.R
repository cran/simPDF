## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(simPDF)
out <- function() tempfile(fileext = ".pdf")   # examples write to a temp file

## ----quickstart---------------------------------------------------------------
doc <- sp_new(out(), paper = "letter")
flow_run(doc, list(
  block_para("Hello, simPDF", size = 16, font = 2),   # font: 1 plain, 2 bold, 3 italic
  block_rule(),
  block_para("This paragraph is measured and wraps to the frame width automatically.
              Add as much text as you like; the engine breaks lines and pages for you.")
))
sp_close(doc)

## ----text---------------------------------------------------------------------
doc <- sp_new(out())
flow_run(doc, list(
  block_para("Left / centre / right alignment", size = 12, font = 2),
  block_para("centred", align = "center"),
  block_para("right",   align = "right"),
  block_para("A fixed-point summary printed verbatim:", size = 11, font = 2),
  block_pre(capture.output(summary(1:100)))
))
sp_close(doc)

## ----table--------------------------------------------------------------------
big <- data.frame(ID = 1:200, TIME = round(runif(200, 0, 24), 2),
                  DV = round(rlnorm(200), 3), WT = round(runif(200, 50, 90), 1))
doc <- sp_new(out())
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)
flow_run(doc, list(block_para("Input data", 13, font = 2), block_table(big, size = 9)))
cat("pages:", doc$page_no, "\n")
sp_close(doc)

## ----matrix-------------------------------------------------------------------
M <- matrix(round(rnorm(144, 0, .4), 3), 12, 12,
            dimnames = list(paste0("Eta", 1:12), paste0("Eta", 1:12)))
doc <- sp_new(out()); frame_set(doc, top = doc$H-45, bottom = 45, left = 45, right = doc$W-45)
flow_run(doc, list(block_para("Omega matrix (12 x 12)", 13, font = 2),
                   block_matrix(M, size = 9)))
sp_close(doc)

## ----plot---------------------------------------------------------------------
set.seed(1); pred <- rlnorm(80); dv <- pred * exp(rnorm(80, 0, .3))
doc <- sp_new(out()); frame_set(doc, top = doc$H-45, bottom = 45, left = 45, right = doc$W-45)
flow_run(doc, list(
  block_keep(list(
    block_para("Goodness of fit", 12, font = 2),
    block_plot({ plot(pred, dv); abline(0, 1, lty = 3) }, height = 220)))
))
sp_close(doc)

## ----figpage------------------------------------------------------------------
doc <- sp_new(out())
for (s in 1:3)
  sp_figure_page(doc, { for (i in 1:6) plot(rnorm(30), rnorm(30), main = paste("Subj", s)) },
                 mfrow = c(2, 3))
cat("pages:", doc$page_no, "\n")
sp_close(doc)

## ----structure----------------------------------------------------------------
doc <- sp_new(out()); frame_set(doc, top = doc$H-55, bottom = 45, left = 45, right = doc$W-45)
flow_run(doc,
  blocks = list(block_para("Report body", 12), block_spacer(10),
                block_para(paste(rep("More text.", 40), collapse = " "))),
  footer = block_para("CONFIDENTIAL", size = 8, align = "center"))
sp_close(doc)

## ----qa-----------------------------------------------------------------------
doc <- sp_new(out()); frame_set(doc, top = doc$H-45, bottom = 45, left = 45, right = doc$W-45)
sp_trace(doc, TRUE)
flow_run(doc, list(block_para("Measured & overlap-free", 14, font = 2),
                   block_table(head(mtcars))))
cat("Courier '12345' at 10pt =", sp_width(doc, "12345", size = 10, family = "mono"), "pt\n")
cat("overlapping text boxes  =", nrow(sp_overlaps(doc)), "\n")
sp_close(doc)

## ----signature----------------------------------------------------------------
f <- out()
doc <- sp_new(f); frame_set(doc, top = doc$H-50, bottom = 60, left = 54, right = doc$W-54)
flow_run(doc, list(
  block_para("Validation Report", 16, font = 2), block_rule(),
  block_authorship("Kyun-Seop Bae", role = "Prepared by",
                   affiliation = "Asan Medical Center", date = "2026-07-11"),
  block_spacer(30), block_para("Approvals", 12, font = 2),
  block_signature(c("Performed by", "Reviewed by"))
))
sp_close(doc)
sp_add_sig_fields(f, doc)          # add clickable /Sig fields over the drawn lines

## ----flowdiagram--------------------------------------------------------------
nodes <- data.frame(
  id = c("001","002","003","004","005"),
  parent = c("", "001", "001", "002", "002"),
  label = c("Run 001\nOFV 1000", "Run 002\nOFV 980", "Run 003\nOFV 975",
            "Run 004\nOFV 960", "Run 005\nOFV 955"),
  stringsAsFactors = FALSE)
doc <- sp_new(out()); frame_set(doc, top = doc$H-45, bottom = 45, left = 40, right = doc$W-40)
flow_run(doc, list(block_para("Model development flow", 14, font = 2),
                   block_flow_diagram(nodes, height = 400)))
sp_close(doc)

