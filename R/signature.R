# Authorship / signature support for reports.
#
# Two complementary pieces:
#  (1) block_signature() draws VISIBLE signature lines (name + date) natively on
#      the device, and records the signing rectangles on the document handle.
#  (2) sp_add_sig_fields() post-processes the finished PDF to insert interactive
#      Adobe Acrobat digital-signature fields (AcroForm /Sig) at those rectangles,
#      via a pure base-R incremental update (no external tools) -- generalised
#      from NonCompart::addSigFieldNCA. The device cannot emit form fields, so the
#      interactive fields are added as a finishing pass over the written file.

#' A signature / approval block (visible lines + optional interactive fields)
#'
#' Draws one signing line per role with a caption beneath and, when
#' \code{with_date}, a dated line to the right. Each signing rectangle is
#' recorded on \code{doc} (in \code{doc$sig}) so \code{\link{sp_add_sig_fields}}
#' can place a clickable Acrobat signature field exactly over it after the PDF
#' is written.
#'
#' @param roles character vector of signer roles (e.g. "Performed by")
#' @param with_date add a dated line beside each signature
#' @param size,caption_size text sizes in points
#' @param sig_height height of the signing space in points
#' @param sig_frac,date_frac signature/date line widths as fractions of the frame
#' @param gap vertical gap between rows in points
#' @param field record interactive-field rectangles on the document
#' @return A block object: a \code{list} with components \code{measure},
#'   \code{draw}, \code{keep} and \code{split} that the flow engine
#'   understands. Pass it to \code{\link{flow_run}} or \code{\link{flow_add}}
#'   to place it in the document; when \code{field = TRUE} drawing also
#'   records each signing rectangle in \code{doc$sig} for
#'   \code{\link{sp_add_sig_fields}}.
#' @export
block_signature <- function(roles = c("Performed by", "Reviewed by"),
                            with_date = TRUE, size = 10, caption_size = 8,
                            sig_height = 26, sig_frac = 0.5, date_frac = 0.22,
                            gap = 16, field = TRUE) {
  n <- length(roles)
  rowh <- sig_height + caption_size * 1.5 + gap
  list(
    keep = FALSE, split = NULL,
    measure = function(doc, width) n * rowh,
    draw = function(doc, x, y, width) {
      grDevices::dev.set(doc$dev)
      sigw  <- width * sig_frac
      datew <- width * date_frac
      datex <- x + width - datew
      for (i in seq_len(n)) {
        yline <- y - (i - 1L) * rowh - sig_height          # signing baseline
        graphics::lines(c(x, x + sigw), c(yline, yline), xpd = NA)
        .draw_text(doc, x, yline - 2, roles[i], caption_size, 1L, doc$family)
        if (with_date) {
          graphics::lines(c(datex, datex + datew), c(yline, yline), xpd = NA)
          .draw_text(doc, datex, yline - 2, "Date (YYYY-MM-DD)", caption_size, 1L, doc$family)
        }
        if (isTRUE(field))
          doc$sig <- c(doc$sig, list(list(
            page = doc$page_no,
            name = gsub("[^A-Za-z0-9]+", "_", trimws(roles[i])),
            rect = c(x, yline, x + sigw, yline + sig_height))))
      }
      invisible()
    })
}

#' An authorship block (prepared-by line + metadata)
#'
#' A compact block naming who prepared the document and when, plus optional
#' affiliation and free-form lines. Purely visible text.
#'
#' @param author character, author name
#' @param role character, e.g. "Prepared by"
#' @param affiliation optional affiliation line
#' @param date optional date string (character); no default is stamped
#' @param extra optional character vector of extra lines
#' @param size font size in points
#' @return A block object (a \code{\link{block_pre}} under the hood): a
#'   \code{list} with components \code{measure}, \code{draw}, \code{keep} and
#'   \code{split} that the flow engine understands. Pass it to
#'   \code{\link{flow_run}} or \code{\link{flow_add}} to place it in the
#'   document.
#' @export
block_authorship <- function(author, role = "Prepared by", affiliation = NULL,
                             date = NULL, extra = NULL, size = 10) {
  lines <- paste0(role, ": ", author)
  if (!is.null(affiliation)) lines <- c(lines, affiliation)
  if (!is.null(date))        lines <- c(lines, paste0("Date: ", date))
  if (!is.null(extra))       lines <- c(lines, extra)
  block_pre(lines, size = size, family = "sans")
}

#' Add interactive Acrobat signature fields to a finished PDF
#'
#' Inserts empty AcroForm digital-signature (\code{/Sig}) fields into a base-R
#' \code{pdf()} document (such as one produced by simPDF) using a pure base-R
#' incremental update -- no external tools or packages. The result can be signed
#' with one click in the free Adobe Acrobat Reader. Generalised from
#' \code{NonCompart::addSigFieldNCA} to accept fields on any page, e.g. the
#' rectangles recorded by \code{\link{block_signature}} in \code{doc$sig}.
#'
#' @param pdf path to the input PDF
#' @param fields a list of fields, each \code{list(page=, name=, rect=c(x0,y0,x1,y1))};
#'   or a document handle whose \code{$sig} list is used
#' @param out output path (default: overwrite \code{pdf})
#' @return The output file path \code{out}, invisibly (a character string).
#'   Called for its side effect of writing the modified PDF, with the
#'   signature fields appended as an incremental update.
#' @export
sp_add_sig_fields <- function(pdf, fields, out = pdf) {
  if (is.environment(fields)) fields <- fields$sig
  if (!length(fields)) stop("no signature fields given", call. = FALSE)
  if (!file.exists(pdf)) stop("PDF not found: ", pdf, call. = FALSE)
  raw <- readBin(pdf, "raw", file.info(pdf)$size); n <- length(raw)
  asc <- function(a, b) rawToChar(raw[a:b])
  objRange <- function(num) {
    hits <- grepRaw(charToRaw(paste0("\n", num, " 0 obj")), raw, all = TRUE)
    if (!length(hits)) stop("object not found: ", num, call. = FALSE)
    s <- hits[length(hits)] + 1L
    e <- grepRaw(charToRaw("endobj"), raw, offset = s, all = FALSE)
    c(s, e + 5L)
  }
  toff <- grepRaw(charToRaw("trailer"), raw, all = TRUE)
  if (!length(toff)) stop("no trailer: cross-reference streams not supported.", call. = FALSE)
  ttxt <- asc(toff[length(toff)], n)
  Size <- as.integer(sub(".*?/Size\\s+(\\d+).*", "\\1", ttxt))
  Root <- as.integer(sub(".*?/Root\\s+(\\d+)\\s+0\\s+R.*", "\\1", ttxt))
  Info <- if (grepl("/Info", ttxt)) as.integer(sub(".*?/Info\\s+(\\d+)\\s+0\\s+R.*", "\\1", ttxt)) else NA
  sx <- grepRaw(charToRaw("startxref"), raw, all = TRUE)
  oldStartxref <- as.integer(sub("^startxref\\s+(\\d+).*", "\\1", asc(sx[length(sx)], n)))

  cr <- objRange(Root); catTxt <- asc(cr[1], cr[2])
  if (grepl("/AcroForm", catTxt)) stop("PDF already has an AcroForm; not modifying.", call. = FALSE)
  pagesNum <- as.integer(sub(".*?/Pages\\s+(\\d+)\\s+0\\s+R.*", "\\1", catTxt))
  pr <- objRange(pagesNum); pagesTxt <- asc(pr[1], pr[2])
  kids <- regmatches(pagesTxt, regexpr("/Kids\\s*\\[[^]]*\\]", pagesTxt))
  kidNums <- as.integer(regmatches(kids, gregexpr("\\d+(?=\\s+0\\s+R)", kids, perl = TRUE))[[1]])

  nf <- length(fields)
  newNums <- Size + seq_len(nf) - 1L
  allRefs <- paste(paste0(newNums, " 0 R"), collapse = " ")

  pos <- n; pieces <- character(0)
  emit <- function(txt) { o <- pos; pieces[[length(pieces) + 1L]] <<- txt
                          pos <<- pos + nchar(txt, type = "bytes"); o }

  # widget objects (one per field), remembering which page each belongs to
  widgetOffs <- integer(nf); pageOfField <- integer(nf)
  for (i in seq_len(nf)) {
    f <- fields[[i]]; r <- f$rect
    pageIdx <- if (is.null(f$page)) 1L else f$page
    if (pageIdx < 1 || pageIdx > length(kidNums)) stop("field page out of range", call. = FALSE)
    pageOfField[i] <- kidNums[pageIdx]
    nm <- if (is.null(f$name)) paste0("Sig_", i) else f$name
    widgetOffs[i] <- emit(sprintf(paste0("%d 0 obj\n<< /Type /Annot /Subtype /Widget /FT /Sig ",
      "/T (%s) /Rect [%g %g %g %g] /P %d 0 R /F 4 /BS << /W 1 /S /S >> /MK << /BC [0 0 0] >> >>\nendobj\n"),
      newNums[i], nm, r[1], r[2], r[3], r[4], kidNums[pageIdx]))
  }
  # updated catalog with one AcroForm listing all fields
  catNew <- sub(">>\\s*endobj\\s*$",
                sprintf("/AcroForm << /Fields [%s] /SigFlags 3 >> >>\nendobj\n", allRefs), catTxt)
  catOff <- emit(catNew)
  # updated page objects: add this page's field refs to its /Annots
  pageOffs <- c(); changedPages <- unique(pageOfField)
  for (pn in changedPages) {
    prng <- objRange(pn); pageTxt <- asc(prng[1], prng[2])
    refs <- paste(paste0(newNums[pageOfField == pn], " 0 R"), collapse = " ")
    if (grepl("/Annots", pageTxt)) pageNew <- sub("/Annots\\s*\\[", paste0("/Annots [", refs, " "), pageTxt)
    else pageNew <- sub(">>\\s*endobj\\s*$", sprintf("/Annots [%s] >>\nendobj\n", refs), pageTxt)
    if (!grepl("endobj\\s*$", pageNew)) pageNew <- paste0(pageNew, "\n")
    pageOffs[as.character(pn)] <- emit(pageNew)
  }

  xrefOff <- pos
  offMap <- c(); offMap[as.character(Root)] <- catOff
  for (pn in changedPages) offMap[as.character(pn)] <- pageOffs[as.character(pn)]
  for (k in seq_len(nf)) offMap[as.character(newNums[k])] <- widgetOffs[k]
  chg <- sort(unique(c(Root, changedPages, newNums)))
  xref <- "xref\n"; i <- 1L
  while (i <= length(chg)) {
    j <- i; while (j < length(chg) && chg[j + 1L] == chg[j] + 1L) j <- j + 1L
    run <- chg[i:j]
    xref <- paste0(xref, sprintf("%d %d\n", run[1], length(run)),
                   paste(sprintf("%010d 00000 n \n", offMap[as.character(run)]), collapse = ""))
    i <- j + 1L
  }
  newSize <- max(Size, max(newNums) + 1L)
  trailer <- sprintf("trailer\n<< /Size %d /Root %d 0 R%s /Prev %d >>\nstartxref\n%d\n%%%%EOF\n",
                     newSize, Root, if (!is.na(Info)) sprintf(" /Info %d 0 R", Info) else "",
                     oldStartxref, xrefOff)
  writeBin(c(raw, charToRaw(paste0(paste(pieces, collapse = ""), xref, trailer))), out)
  invisible(normalizePath(out, winslash = "/", mustWork = FALSE))
}
