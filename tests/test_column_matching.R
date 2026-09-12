# Regression test for guess_column(). Run from the project root:
#   Rscript tests/test_column_matching.R
#
# guess_column() used to strip [^a-z0-9] BEFORE lowercasing, which deleted every
# capital letter in a header ("CFU" -> "") instead of folding it. Exits non-zero
# on failure.
app_lines <- readLines("app.R", warn = FALSE)
ui_start <- grep("^ui <- fluidPage", app_lines)[1]
eval(parse(text = paste(app_lines[1:(ui_start - 1)], collapse = "\n")), envir = globalenv())

CANDS <- list(
  sample        = c("sample", "strain", "vector", "construct", "plasmid", "genotype", "group"),
  concentration = c("treatment", "condition", "dose", "concentration", "inducer", "induction", "iptg", "arabinose", "atc"),
  time          = c("time", "timepoint", "minutes", "hours"),
  replicate     = c("replicate", "rep", "biorep", "trial"),
  cfu           = c("cfu", "cfuperml", "cfuml", "count", "colonies", "titer", "titre")
)
map_all <- function(cols) vapply(CANDS, function(cd) guess_column(cols, cd), character(1))

cases <- list(
  "user gp75 file"   = list(cols = c("Sample", "inducer_concentration", "Time", "Replicate", "CFU"),
                            want = c("Sample", "inducer_concentration", "Time", "Replicate", "CFU")),
  "bundled demo"     = list(cols = c("Group", "Treatment", "Timepoint", "Replicate", "CFU"),
                            want = c("Group", "Treatment", "Timepoint", "Replicate", "CFU")),
  "leading ID col"   = list(cols = c("ID", "Sample", "inducer_concentration", "Time", "Replicate", "CFU"),
                            want = c("Sample", "inducer_concentration", "Time", "Replicate", "CFU")),
  "all lowercase"    = list(cols = c("strain", "iptg_uM", "time_min", "rep", "cfu_per_mL"),
                            want = c("strain", "iptg_uM", "time_min", "rep", "cfu_per_mL")),
  "spaces + caps"    = list(cols = c("Strain Name", "IPTG (uM)", "Time (min)", "Bio Rep", "CFU/mL"),
                            want = c("Strain Name", "IPTG (uM)", "Time (min)", "Bio Rep", "CFU/mL")),
  # Real CFU Calculator raw export. It has no separate time or dose column
  # (both are baked into the sample string), so those two honestly have no
  # answer and fall back to the first column.
  "CFU Calc export"  = list(cols = c("experiment", "date", "sample", "replicate", "dilution_exponent",
                                     "dilution_factor", "volume_uL", "colonies", "flag", "cfu_per_ml"),
                            want = c("sample", "experiment", "experiment", "replicate", "cfu_per_ml"))
)

fails <- 0
for (nm in names(cases)) {
  got <- unname(map_all(cases[[nm]]$cols))
  want <- cases[[nm]]$want
  ok <- identical(got, want)
  if (!ok) fails <- fails + 1
  cat(sprintf("%-16s %s\n", nm, if (ok) "PASS" else "FAIL"))
  if (!ok) {
    print(data.frame(field = names(CANDS), got = got, want = want))
  }
}
cat(sprintf("\n%d/%d cases pass\n", length(cases) - fails, length(cases)))

# ---------------------------------------------------------------------------
# CPS-9: CSV dialects
# ---------------------------------------------------------------------------
# A European export writes 1,5E+08 with ";" separators. The default read_csv()
# locale parsed that to a character column, then to NA, and every row was
# dropped by the !is.na(cfu) filter.
cat("\n-- CSV dialects (CPS-9) --\n")
dfails <- 0
dok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { dfails <<- dfails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}

write_tmp <- function(txt, ext = ".csv") {
  f <- tempfile(fileext = ext); writeLines(txt, f); f
}

f_comma <- write_tmp(c("Sample,Treatment,Time,Replicate,CFU",
                       "A,0,0,R1,1.5E+08",
                       "A,0,0,R2,2.5E+08"))
f_semi <- write_tmp(c("Sample;Treatment;Time;Replicate;CFU",
                      "A;0;0;R1;1,5E+08",
                      "A;0;0;R2;2,5E+08"))
f_tab <- write_tmp(c("Sample\tTreatment\tTime\tReplicate\tCFU",
                     "A\t0\t0\tR1\t1.5E+08",
                     "A\t0\t0\tR2\t2.5E+08"), ".tsv")

dok("a plain CSV is detected as comma", identical(detect_csv_format(f_comma), "comma"))
dok("a semicolon file is detected", identical(detect_csv_format(f_semi), "semicolon"))
dok("a tab file is detected", identical(detect_csv_format(f_tab), "tab"))

want <- c(1.5e8, 2.5e8)
for (nm in c("comma", "semicolon", "tab")) {
  f <- list(comma = f_comma, semicolon = f_semi, tab = f_tab)[[nm]]
  r <- read_cfu_csv(f, "auto")
  dok(sprintf("%-10s parses five columns", nm), ncol(r) == 5, paste("got", ncol(r)))
  dok(sprintf("%-10s parses CFU as a number", nm), is.numeric(r$CFU),
      paste(class(r$CFU), collapse = "/"))
  dok(sprintf("%-10s reads the right values", nm),
      isTRUE(all.equal(as.numeric(r$CFU), want)))
  dok(sprintf("%-10s records the dialect it used", nm), identical(attr(r, "csv_format"), nm))
}

# The whole point: the decimal-comma file must survive prep_cfu_data, which is
# where the NAs used to be dropped.
prepped <- prep_cfu_data(
  read_cfu_csv(f_semi, "auto"),
  list(sample = "Sample", concentration = "Treatment", time = "Time",
       replicate = "Replicate", bio_replicate = "Replicate", cfu = "CFU"),
  "", "min", FALSE, TRUE)
dok("a decimal-comma file survives prep with both rows", nrow(prepped) == 2,
    paste("rows", nrow(prepped)))
dok("and with correct log10 values",
    isTRUE(all.equal(prepped$log10_cfu, log10(want))))

# An explicit dialect overrides detection, which is the escape hatch for a file
# the sniffer gets wrong.
dok("an explicit dialect is honoured over detection",
    identical(attr(read_cfu_csv(f_semi, "semicolon"), "csv_format"), "semicolon"))
dok("reading a semicolon file as comma gives one wide column",
    ncol(read_cfu_csv(f_semi, "comma")) == 1)

unlink(c(f_comma, f_semi, f_tab))
cat(sprintf("\n%s\n", if (dfails == 0) "ALL CSV DIALECT CHECKS PASSED" else sprintf("%d CSV DIALECT CHECK(S) FAILED", dfails)))

if (fails > 0 || dfails > 0) quit(status = 1)
