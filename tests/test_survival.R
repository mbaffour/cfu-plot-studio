# Paired survival readout tests. Run from the project root:
#   Rscript tests/test_survival.R
#
# Expected values are hand-computed or derived independently of the app's own
# helpers, so an implementation that is wrong in a self-consistent way still
# fails here. Exits non-zero on failure.

suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(tidyr)})
app_lines <- readLines("app.R", warn = FALSE)
ui_start <- grep("^ui <- fluidPage", app_lines)[1]
eval(parse(text = paste(app_lines[1:(ui_start - 1)], collapse = "\n")), envir = globalenv())

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}
eq <- function(a, b, tol = 1e-9) isTRUE(all.equal(unname(a), unname(b), tolerance = tol))

prep <- function(df) prep_cfu_data(df, list(sample = "Sample", concentration = "inducer_concentration",
                                            time = "Time", replicate = "Replicate", cfu = "CFU"),
                                   "", "min", FALSE, TRUE)

# --- fixture with exactly known survival ------------------------------------
# A@0  : 1e8 -> 1e7 x3            every ratio -1    (10% survival, sd 0)
# A@10 : 1e8 -> 1e8 x3            every ratio  0    (no change)
# B@0  : 1e8 -> 1e6,1e7,1e8       ratios -2,-1,0    (mean -1, sd 1)
# B@10 : 1e8 -> 1e9 for rep1 only (one pair, two orphans)
fixture <- tibble::tribble(
  ~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "A", 0,  0, 1, 1e8,  "A", 0,  0, 2, 1e8,  "A", 0,  0, 3, 1e8,
  "A", 0,120, 1, 1e7,  "A", 0,120, 2, 1e7,  "A", 0,120, 3, 1e7,
  "A",10,  0, 1, 1e8,  "A",10,  0, 2, 1e8,  "A",10,  0, 3, 1e8,
  "A",10,120, 1, 1e8,  "A",10,120, 2, 1e8,  "A",10,120, 3, 1e8,
  "B", 0,  0, 1, 1e8,  "B", 0,  0, 2, 1e8,  "B", 0,  0, 3, 1e8,
  "B", 0,120, 1, 1e6,  "B", 0,120, 2, 1e7,  "B", 0,120, 3, 1e8,
  "B",10,  0, 1, 1e8,  "B",10,  0, 2, 1e8,  "B",10,  0, 3, 1e8,
  "B",10,120, 1, 1e9
)
fx <- prep(fixture)
sv <- pair_survival(fx, "0 min", "120 min")

cat("\n-- pairing --\n")
ok("only complete pairs are kept", nrow(sv) == 10, paste("got", nrow(sv)))
ok("frame is marked as survival", is_survival_frame(sv))
ok("a raw frame is NOT marked as survival", !is_survival_frame(fx))
ok("log ratio is readout minus baseline",
   eq(sv$log10_cfu, sv$readout_log10 - sv$baseline_log10))
ok("cfu holds the fold change and stays positive",
   eq(sv$cfu, 10^sv$log10_cfu) && all(sv$cfu > 0))
ok("one row per (sample, treatment, replicate)",
   nrow(distinct(sv, sample, concentration_label, replicate)) == nrow(sv))

g <- sv %>% group_by(sample, concentration_label) %>%
  summarise(n = n(), m = mean(log10_cfu), s = sd(log10_cfu), .groups = "drop")
ok("A@0  = 3 pairs, mean -1, sd 0", with(g[1, ], n == 3 && eq(m, -1) && eq(s, 0)))
ok("A@10 = 3 pairs, mean  0, sd 0", with(g[2, ], n == 3 && eq(m, 0) && eq(s, 0)))
ok("B@0  = 3 pairs, mean -1, sd 1", with(g[3, ], n == 3 && eq(m, -1) && eq(s, 1)))
ok("B@10 = 1 pair,  mean  1, sd NA", with(g[4, ], n == 1 && eq(m, 1) && is.na(s)))

qc <- survival_pairing_qc(fx, "0 min", "120 min")
ok("QC counts complete pairs", qc$complete == 10)
ok("QC counts the two orphaned baselines", qc$baseline_only == 2)
ok("QC counts zero orphaned readouts", qc$readout_only == 0)
ok("QC reports no empty cells here", length(qc$empty_cells) == 0)

cat("\n-- pairing is by replicate, not by position --\n")
# Scale BOTH counts of one replicate: a within-replicate ratio must not move.
scaled <- fixture; i <- which(scaled$Sample == "B" & scaled$inducer_concentration == 0 & scaled$Replicate == 1)
scaled$CFU[i] <- scaled$CFU[i] * 7.3
sv2 <- pair_survival(prep(scaled), "0 min", "120 min")
ok("multiplying both of a replicate's timepoints leaves every ratio unchanged",
   eq(sort(sv2$log10_cfu), sort(sv$log10_cfu)))
# Shuffling which readout belongs to which replicate MUST change the answer.
shuf <- fixture
j <- which(shuf$Sample == "B" & shuf$inducer_concentration == 0 & shuf$Time == 120)
shuf$CFU[j] <- rev(shuf$CFU[j])
sv3 <- pair_survival(prep(shuf), "0 min", "120 min")
ok("a ratio-of-means implementation would not notice a replicate permutation; this does",
   !eq(sv3$log10_cfu[sv3$sample == "B" & sv3$concentration_label == "0"],
       sv$log10_cfu[sv$sample == "B" & sv$concentration_label == "0"]))

cat("\n-- Jensen: mean of ratios is not the ratio of arithmetic means --\n")
jen <- tibble::tribble(
  ~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "A", 0, 0, 1, 1e8,  "A", 0, 0, 2, 1e10,
  "A", 0,120, 1, 1e7,  "A", 0,120, 2, 1e10)
jv <- pair_survival(prep(jen), "0 min", "120 min")
ok("paired mean log ratio is -0.5", eq(mean(jv$log10_cfu), -0.5))
ok("10^mean is the GEOMETRIC mean fold change", eq(10^mean(jv$log10_cfu), sqrt(0.1 * 1)))
ok("and is NOT the ratio of arithmetic means",
   !eq(mean(jv$log10_cfu), log10(mean(c(1e7, 1e10))) - log10(mean(c(1e8, 1e10)))))

cat("\n-- orphans change the estimate (the marginal estimator does not discard them) --\n")
orp <- tibble::tribble(
  ~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "A", 0, 0, 1, 1e8,  "A", 0, 0, 2, 1e10,
  "A", 0,120, 1, 1e7)
ov <- pair_survival(prep(orp), "0 min", "120 min")
ok("orphan is dropped, leaving one pair", nrow(ov) == 1 && eq(ov$log10_cfu, -1))
marginal <- log10(1e7) - mean(log10(c(1e8, 1e10)))
ok("the marginal estimator would report -2, twice the real kill", eq(marginal, -2))
ok("paired and marginal differ by a full log here", eq(ov$log10_cfu - marginal, 1))

cat("\n-- the clamp regression: negative error bars must survive --\n")
sm <- plot_summary(sv, "survival", "log10", "SD", clamp_zero = FALSE)
b0 <- sm %>% filter(sample == "B", concentration_label == "0")
ok("lower whisker keeps its negative value", eq(b0$ymin, -2), paste("got", b0$ymin))
ok("ymin < ymax on every row", all(sm$ymin <= sm$ymax, na.rm = TRUE))
smc <- plot_summary(sv, "survival", "log10", "SD", clamp_zero = TRUE)
ok("with the legacy clamp that same whisker is destroyed", eq(smc$ymin[smc$sample == "B" & smc$concentration_label == "0"], 0))
ok("absolute-CFU mode still clamps at zero (unchanged behaviour)",
   eq(plot_summary(fx, "sample_time", "log10", "SD")$ymin[1] >= 0, TRUE))
# Survival ignores raw_log_axis entirely (the value is already a log ratio), so
# the eps floor is tested where it still applies: absolute counts.
ok("survival mode ignores raw_log_axis and stays on the signed log scale",
   eq(plot_summary(sv, "survival", "raw_log_axis", "SD", clamp_zero = FALSE)$ymin,
      plot_summary(sv, "survival", "log10", "SD", clamp_zero = FALSE)$ymin))
ok("raw_log_axis floor still applies to absolute counts",
   all(plot_summary(fx, "combined", "raw_log_axis", "SD")$ymin > 0, na.rm = TRUE))

cat("\n-- one-sample survival statistics --\n")
st <- run_survival_stats(sv, "survival_vs_zero", "none", "global", "0", "welch")
row <- function(s, c) st[st$sample == s & as.character(st$concentration_label) == c, ]
r <- row("B", "0")
ok("B@0 estimate is -1", eq(r$estimate_log10_difference, -1))
ok("B@0 p matches an independent one-sample t", eq(r$p.value, t.test(c(-2, -1, 0), mu = 0)$p.value))
ok("B@0 t statistic is -sqrt(3)", eq(r$statistic, -sqrt(3)))
ok("B@0 df = n-1 = 2", eq(r$parameter, 2))
ok("B@0 CI matches", eq(c(r$conf.low, r$conf.high), t.test(c(-2, -1, 0), mu = 0)$conf.int[1:2]))
ok("B@0 d_z = mean/sd = -1", eq(r$d_z, -1))
ok("effect size is labelled as paired", identical(r$effect_size_kind, "paired (d_z)"))
ok("fold change is 10^estimate", eq(r$fold_change, 0.1))
ok("percent survival is 10%", eq(r$percent_survival, 10))
ok("n_pairs is reported", r$n_pairs == 3)

r <- row("A", "0")
ok("A@0 (every replicate lost exactly 1 log) does not error", nrow(r) == 1)
ok("A@0 keeps its point estimate", eq(r$estimate_log10_difference, -1))
ok("A@0 returns NA p rather than a spurious one", is.na(r$p.value))
ok("A@0 explains why", grepl("identical amount", r$message))
r <- row("A", "10")
ok("A@10 (no change at all, constant AT the null) also returns NA not NaN", is.na(r$p.value))
r <- row("B", "10")
ok("single-pair cell gives a point estimate and no inference",
   r$n_pairs == 1 && eq(r$estimate_log10_difference, 1) && is.na(r$p.value))
ok("single-pair cell says so", grepl("no inference", r$message))
ok("no NaN reaches the p column", !any(is.nan(st$p.value)))

cat("\n-- d_z is not the two-sample g --\n")
base <- c(9, 10, 11); read <- c(8.4, 9.6, 10.5)
dz <- paired_dz(read - base)
ok("d_z uses the SD of differences", eq(dz, mean(read - base) / sd(read - base)))
ok("d_z and two-sample g differ by a large factor on the same data",
   abs(dz) > 4 * abs(hedges_g(read, base)))

cat("\n-- delegated comparisons run on the ratios --\n")
sc <- run_survival_stats(sv, "sample", "none", "global", "0", "welch")
ok("between-sample comparison returns rows", nrow(sc) > 0)
ok("its test names the survival ratio", any(grepl("survival ratio", sc$test)))
ok("it is labelled as a two-sample effect size", all(sc$effect_size_kind == "two-sample (Hedges g)"))
a0 <- sv$log10_cfu[sv$sample == "A" & sv$concentration_label == "0"]
b0v <- sv$log10_cfu[sv$sample == "B" & sv$concentration_label == "0"]
ok("its p matches a direct Welch test on the two ratio sets",
   eq(sc$p.value[as.character(sc$concentration_label) == "0"], t.test(a0, b0v)$p.value))
blocked <- run_survival_stats(sv, "time", "none", "global", "0", "welch")
ok("the timepoint comparison is refused, not silently empty",
   "message" %in% names(blocked) && grepl("already been consumed", blocked$message[1]))
ok("raw frames are refused by the survival stats",
   grepl("require a paired survival frame", run_survival_stats(fx, "survival_vs_zero", "none", "global", "0", "welch")$message[1]))

cat("\n-- axis and annotation --\n")
ok("auto comparison for survival mode", identical(resolve_auto_comparison("survival"), "survival_vs_zero"))
ok("auto comparison unchanged elsewhere",
   identical(resolve_auto_comparison("combined"), "sample") &&
   identical(resolve_auto_comparison("sample_both"), "time"))
lbl <- survival_scale_labeller("percent")
ok("percent labels are the same numbers relabelled", identical(lbl(c(-1, 0, 1)), c("10%", "100%", "1000%")),
   paste(lbl(c(-1, 0, 1)), collapse = " "))
ok("fold labels likewise", identical(survival_scale_labeller("fold")(c(-1, 0, 1)), c("0.1x", "1x", "10x")),
   paste(survival_scale_labeller("fold")(c(-1, 0, 1)), collapse = " "))
ok("labels are not padded to a common width",
   !any(grepl("^ ", survival_scale_labeller("fold")(c(-2, 0, 2)))))
ann <- annotation_data(st, sm, "survival_vs_zero", "survival", "stars", TRUE, "log10", dodge_width = 0.8)
# Cells where no test could run carry no label, so the count is the number of
# TESTABLE cells, not the number of bars: in this fixture A@0 and A@10 have zero
# spread and B@10 has a single pair, leaving only B@0.
ok("one annotation per testable bar, none for the untestable ones",
   nrow(ann) == sum(!is.na(st$significance)),
   paste(nrow(ann), "vs", sum(!is.na(st$significance))))
ok("untestable cells are left unlabelled rather than drawn at NA",
   nrow(ann) == 1 && identical(as.character(ann$sample), "B"))

# Dodging can only be observed where BOTH samples at one x are testable.
dodge_fix <- tibble::tribble(
  ~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "A", 0,  0, 1, 1e8,  "A", 0,  0, 2, 1e8,  "A", 0,  0, 3, 1e8,
  "A", 0,120, 1, 1e6,  "A", 0,120, 2, 1e7,  "A", 0,120, 3, 3e7,
  "B", 0,  0, 1, 1e8,  "B", 0,  0, 2, 1e8,  "B", 0,  0, 3, 1e8,
  "B", 0,120, 1, 1e9,  "B", 0,120, 2, 3e9,  "B", 0,120, 3, 2e9)
dv <- pair_survival(prep(dodge_fix), "0 min", "120 min")
dsm <- plot_summary(dv, "survival", "log10", "SD", clamp_zero = FALSE)
dst <- run_survival_stats(dv, "survival_vs_zero", "none", "global", "0", "welch")
dann <- annotation_data(dst, dsm, "survival_vs_zero", "survival", "stars", TRUE, "log10", dodge_width = 0.8)
ok("both samples at one x get a label", nrow(dann) == 2)
ok("their labels are dodged apart, not stacked",
   length(unique(round(dann$x_pos, 6))) == 2, paste(round(dann$x_pos, 4), collapse = ", "))
ok("the dodge is symmetric about the tick", eq(sum(dann$x_pos), 2))

idx <- match(paste(ann$sample, ann$x), paste(sm$sample, sm$concentration_label))
top <- pmax(sm$ymax[idx], sm$mean_y[idx], na.rm = TRUE)
ok("annotation sits above its own bar", all(is.finite(ann$y)) && all(ann$y > top - 1e-9))
ok("no annotation is placed at -Inf (the single-pair cell)", all(is.finite(ann$y)))

cat("\n-- the negative-pad hazard on a log axis --\n")
allneg <- sm; allneg$ymax <- -abs(allneg$ymax) - 0.2; allneg$ymin <- allneg$ymax - 0.3
a2 <- annotation_data(st, allneg, "survival_vs_zero", "survival", "stars", TRUE, "raw_log_axis")
# The original bug: pad = 0.25 * max(ymax) goes NEGATIVE when every group sits
# below the reference, pushing each label UNDER its own bar. Labels are placed
# per group, so the check is against each label's own group top.
idx2 <- match(paste(a2$sample, a2$x), paste(allneg$sample, allneg$concentration_label))
own_top <- pmax(allneg$ymax[idx2], allneg$mean_y[idx2], na.rm = TRUE)
ok("labels stay ABOVE their own bar when every group is below the reference",
   nrow(a2) == 0 || (all(is.finite(a2$y)) && all(a2$y > own_top - 1e-9)),
   paste("y:", paste(round(a2$y, 3), collapse = ","), " own_top:", paste(round(own_top, 3), collapse = ",")))
ok("the pad stayed positive despite an all-negative maximum",
   nrow(a2) == 0 || all(a2$y - own_top > 0))

cat("\n-- regressions caught by the adversarial review --\n")

# A blank replicate label must not be joined NA-to-NA into a fabricated pair.
na_rep <- tibble::tribble(~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "S",0,0,"r1",1e6, "S",0,0,"r2",1e6, "S",0,0,"r3",1e6, "S",0,0,NA,1e9,
  "S",0,120,"r1",1e3,"S",0,120,"r2",1e3,"S",0,120,"r3",1e3,"S",0,120,NA,1e8)
nd <- prep(na_rep)
np <- pair_survival(nd, "0 min", "120 min")
nq <- survival_pairing_qc(nd, "0 min", "120 min")
ok("unlabelled wells do not form a pair", nrow(np) == 3, paste("got", nrow(np)))
ok("the surviving estimate is the real one", eq(mean(np$log10_cfu), -3))
ok("unlabelled wells are counted, not silently dropped", nq$unlabelled == 2, paste("got", nq$unlabelled))
ok("they are not miscounted as orphans", nq$baseline_only == 0 && nq$readout_only == 0)
nst <- run_survival_stats(np, "survival_vs_zero", "none", "global", "0", "welch")
ok("a constant-difference cell stays untestable rather than gaining a p", is.na(nst$p.value))

# A survival summary has no time_min column; grouping by it used to hard-error
# the figure and every export.
t2 <- run_survival_stats(dv, "sample", "none", "global", "0", "welch")
r2 <- tryCatch(annotation_data(t2, dsm, "sample", "survival", "stars", TRUE, "log10"),
               error = function(e) e)
ok("between-sample stats annotate a survival figure without erroring", !inherits(r2, "error"),
   if (inherits(r2, "error")) conditionMessage(r2) else "")

# A log ratio must never be sent through the raw-CFU log axis a second time.
ok("survival forces the log10 axis regardless of y_mode",
   eq(plot_summary(dv, "survival", "raw_log_axis", "SD", clamp_zero = FALSE)$mean_y,
      plot_summary(dv, "survival", "log10", "SD", clamp_zero = FALSE)$mean_y))

# A finite ymax must always outrank a sibling group's mean when placing labels.
reg <- data.frame(
  Sample = c(rep("A",3), rep("B",3), "C"),
  inducer_concentration = rep("X", 7), Time = 0, Replicate = c(1,2,3,1,2,3,1),
  CFU = c(1e8,1.1e8,0.9e8, 1e6,1.1e6,0.9e6, 1e10), stringsAsFactors = FALSE)
rd <- prep(reg)
rsm <- plot_summary(rd, "combined", "log10", "SD")
rst <- run_groupwise_t_tests(rd, "sample", "BH", "global", "X", "student")
ra <- annotation_data(rst, rsm, "sample", "combined", "stars", TRUE, "log10")
ok("a single-replicate sibling does not lift a valid star off its own bars",
   eq(ra$y[1], max(rsm$ymax, na.rm = TRUE) + 0.45),
   paste("y =", round(ra$y[1], 4), "expected", round(max(rsm$ymax, na.rm = TRUE) + 0.45, 4)))

cat("\n-- the real gp75 file --\n")
raw <- readr::read_csv("../gp75 cfu all reps_vault dummy.csv", show_col_types = FALSE, trim_ws = TRUE)
if (!inherits(raw, "try-error") && nrow(raw) > 0) {
  names(raw) <- clean_names(names(raw))
  gd <- prep(raw)
  gs <- pair_survival(gd, "0 min", "120 min")
  gq <- survival_pairing_qc(gd, "0 min", "120 min")
  ok("24 complete pairs", nrow(gs) == 24, paste("got", nrow(gs)))
  ok("5 baseline-only orphans", gq$baseline_only == 5, paste("got", gq$baseline_only))
  ok("6 readout-only orphans", gq$readout_only == 6, paste("got", gq$readout_only))
  ok("gp75 @ 0 has no pairs and is named as absent",
     any(grepl("gp75 @ 0", gq$empty_cells)), paste(gq$empty_cells, collapse = "; "))
  # The dose where paired and marginal disagree on the sign of the biology.
  p125 <- gs$log10_cfu[gs$sample == "pSJExD" & gs$concentration_label == "12.5"]
  marg <- mean(gd$log10_cfu[gd$sample == "pSJExD" & gd$concentration_label == "12.5" & gd$time_min == "120 min"]) -
          mean(gd$log10_cfu[gd$sample == "pSJExD" & gd$concentration_label == "12.5" & gd$time_min == "0 min"])
  ok("pSJExD @ 12.5 paired says killing", mean(p125) < 0)
  ok("the marginal estimator says growth at that same dose", marg > 0)
  ok("they disagree on the sign", sign(mean(p125)) != sign(marg))
} else {
  cat("  SKIP  gp75 file not found next to the repo\n")
}

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL SURVIVAL TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
