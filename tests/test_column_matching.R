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
if (fails > 0) quit(status = 1)
