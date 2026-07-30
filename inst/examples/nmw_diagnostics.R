# Example: an nmw-style residual/prediction diagnostics report with inline
# base-R plots (scatter + histogram + QQ) and a summary table, rendered by the
# simPDF flow engine with automatic pagination.
library(simPDF)

set.seed(5)
n <- 120
PRED  <- rlnorm(n)
DV    <- PRED * exp(rnorm(n, 0, 0.3))
CWRES <- rnorm(n)

summ <- data.frame(
  Statistic = c("N", "Min", "Median", "Mean", "Max", "SD"),
  CWRES = round(c(n, min(CWRES), median(CWRES), mean(CWRES), max(CWRES), sd(CWRES)), 3))

doc <- sp_new(tempfile(fileext = ".pdf"), paper = "letter", family = "Helvetica", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)

flow_run(doc, list(
  block_para("S5 - Residual & Prediction Diagnostics", size = 16, font = 2),
  block_rule(),
  block_keep(list(
    block_para("Goodness of fit: DV vs PRED", size = 12, font = 2),
    block_plot({
      plot(PRED, DV, pch = 1, xlab = "PRED", ylab = "DV",
           xlim = range(c(PRED, DV)), ylim = range(c(PRED, DV)))
      abline(0, 1, lty = 3)
    }, height = 230))),
  block_spacer(6),
  block_keep(list(
    block_para("CWRES distribution", size = 12, font = 2),
    block_plot(hist(CWRES, breaks = 20, main = "", xlab = "CWRES", col = "grey85"), height = 180))),
  block_spacer(6),
  block_keep(list(
    block_para("CWRES normal Q-Q", size = 12, font = 2),
    block_plot(qqnorm(CWRES, main = ""), height = 180))),
  block_spacer(6),
  block_para("CWRES summary", size = 12, font = 2),
  block_table(summ, size = 10, family = "mono")),
  footer = block_para("CONFIDENTIAL", size = 8, align = "center"))

sp_close(doc)
