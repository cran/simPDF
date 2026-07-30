# Example: an nmw-style "Summary 2 - Parameters" page rendered with simPDF.
# Demonstrates the overlap fix for large nTheta / nEta results.
library(simPDF)

set.seed(7)
nEta <- 8
OM <- round(matrix(rnorm(nEta * nEta, 0, 0.3), nEta), 3)
OM[lower.tri(OM)] <- t(OM)[lower.tri(OM)]
diag(OM) <- round(runif(nEta, 0.05, 0.6), 3)
dimnames(OM) <- list(paste0("Eta", 1:nEta), paste0("Eta", 1:nEta))
Th <- data.frame(Estimate = round(rnorm(12, 1, 2), 4), SE = round(runif(12, 0.01, 0.5), 4))
Th$LL <- round(Th$Estimate - 2 * Th$SE, 4)
Th$UL <- round(Th$Estimate + 2 * Th$SE, 4)
rownames(Th) <- paste("Theta", 1:12)
EtaCV <- round(sqrt(exp(diag(OM)) - 1) * 100, 2)

doc <- sp_new(tempfile(fileext = ".pdf"), paper = "letter", family = "Courier", size = 10)
frame_set(doc, top = doc$H - 45, bottom = 45, left = 45, right = doc$W - 45)

flow_run(doc, list(
  block_para("Summary 2 - Parameters", size = 16, font = 2),
  block_rule(),
  block_keep(list(
    block_para("Thetas", size = 13, font = 2),
    block_para("Number of All / Fixed / Unfixed Thetas : 12 / 0 / 12", size = 10))),
  block_para("Estimated Thetas", size = 11, font = 2),
  block_pre(capture.output(Th), size = 9),
  block_para("*LL: Lower Limit  UL: Upper Limit  (Estimate +/- 2*SE)", size = 8),
  block_spacer(6),
  block_keep(list(
    block_para("Omegas", size = 13, font = 2),
    block_para(sprintf("Number of Etas : %d", nEta), size = 10))),
  block_para("Omega Matrix (lower=covariance, upper=correlation, diag=variance)", size = 11, font = 2),
  block_pre(capture.output(OM), size = 9),
  block_spacer(6),
  block_para("Interindividual Variability CV(%) for exp(eta) model", size = 11, font = 2),
  block_pre(capture.output(EtaCV), size = 9)
), footer = block_para("CONFIDENTIAL", size = 8, align = "center"))

sp_close(doc)
