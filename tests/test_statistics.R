# Statistical correctness tests. Run from the project root:
#   Rscript tests/test_statistics.R
#
# Each case re-derives the expected number from first principles (or from a
# published worked example) rather than from the app's own helpers, so a bug
# that is consistent across the app still fails here. Exits non-zero on failure.

suppressPackageStartupMessages({library(ggplot2); library(dplyr)})
app_lines <- readLines("app.R", warn = FALSE)
ui_start <- grep("^ui <- fluidPage", app_lines)[1]
eval(parse(text = paste(app_lines[1:(ui_start - 1)], collapse = "\n")), envir = globalenv())

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    cat(sprintf("  PASS  %s\n", label))
  } else {
    fails <<- fails + 1
    cat(sprintf("  FAIL  %s   %s\n", label, detail))
  }
}
eq <- function(a, b, tol = 1e-8) isTRUE(all.equal(unname(a), unname(b), tolerance = tol))

# `bio` says which column identifies the biological replicate: the Replicate
# column by default (what the app's own mapping resolves to), or NA for a file
# whose repeats are technical and unlabelled.
mk <- function(sample, conc, time, rep, cfu, bio = "Replicate") {
  raw <- tibble::tibble(Sample = sample, inducer_concentration = conc,
                        Time = time, Replicate = rep, CFU = cfu)
  prep_cfu_data(raw, list(sample = "Sample", concentration = "inducer_concentration",
                          time = "Time", replicate = "Replicate",
                          bio_replicate = bio, cfu = "CFU"),
                "", "min", FALSE, TRUE)
}

cat("\n-- log10 transform and the non-positive filter --\n")
d <- mk(rep("A", 4), rep(0, 4), rep(0, 4), 1:4, c(1e6, -5, 0, 1e8))
ok("CFU <= 0 and 0 itself are both dropped", nrow(d) == 2, paste("kept", nrow(d)))
ok("log10 is exact", eq(d$log10_cfu, c(6, 8)))

cat("\n-- summary statistics --\n")
vals <- c(1e6, 1e7, 1e8)
d <- mk(rep("A", 3), rep(0, 3), rep(0, 3), 1:3, vals)
sm <- plot_summary(d, "sample_time", "log10", "SD")
lg <- log10(vals)
ok("n", sm$n == 3)
ok("mean of log10, not log10 of mean", eq(sm$mean_y, mean(lg)))
ok("SD is the n-1 sample SD", eq(sm$sd_y, sd(lg)))
ok("SEM = SD/sqrt(n)", eq(sm$sem_y, sd(lg) / sqrt(3)))
ok("mean != log10(mean of raw)", !eq(sm$mean_y, log10(mean(vals))))

sm_ci <- plot_summary(d, "sample_time", "log10", "95% CI")
half <- qt(0.975, 2) * sd(lg) / sqrt(3)
ok("95% CI half-width uses t(0.975, n-1)", eq(sm_ci$ymax - sm_ci$mean_y, half))
ok("95% CI is wider than SD at n=3", (sm_ci$ymax - sm_ci$mean_y) > sm$sd_y)

sm_iqr <- plot_summary(d, "sample_time", "log10", "IQR")
ok("IQR uses quantile type 7", eq(c(sm_iqr$ymin, sm_iqr$ymax),
                                 unname(quantile(lg, c(.25, .75), type = 7))))
sm_rg <- plot_summary(d, "sample_time", "log10", "Range (min-max)")
ok("range is min/max", eq(c(sm_rg$ymin, sm_rg$ymax), c(min(lg), max(lg))))

cat("\n-- geometric mean --\n")
gm <- summary_cfu(d)
ok("geometric_mean_cfu = 10^mean(log10)", eq(gm$geometric_mean_cfu, 10^mean(lg)))
ok("geometric mean != arithmetic mean", !eq(gm$geometric_mean_cfu, mean(vals)))
ok("geometric mean equals prod^(1/n)", eq(gm$geometric_mean_cfu, prod(vals)^(1/3)))

cat("\n-- Welch vs Student --\n")
a <- c(10.0, 10.2, 10.4); b <- c(9.0, 9.9, 10.8)
dd <- mk(c(rep("A", 3), rep("B", 3)), rep(0, 6), rep(0, 6), rep(1:3, 2), 10^c(a, b))
w <- run_groupwise_t_tests(dd, "sample", "none", "global", "0", "welch")
st <- run_groupwise_t_tests(dd, "sample", "none", "global", "0", "student")
ok("Welch p matches t.test(var.equal=FALSE)", eq(w$p.value, t.test(a, b, var.equal = FALSE)$p.value))
ok("Student p matches t.test(var.equal=TRUE)", eq(st$p.value, t.test(a, b, var.equal = TRUE)$p.value))
ok("Welch df is fractional", !eq(w$parameter, round(w$parameter)))
ok("Student df = n1+n2-2", eq(st$parameter, 4))
ok("estimate is the log10 difference", eq(w$estimate_log10_difference, mean(a) - mean(b)))
ok("fold_change = 10^difference", eq(w$fold_change, 10^(mean(a) - mean(b))))
ok("CI matches t.test", eq(c(w$conf.low, w$conf.high), t.test(a, b, var.equal = FALSE)$conf.int[1:2]))
ok("fold-change CI is the transformed CI", eq(c(w$fold_change_low, w$fold_change_high),
                                              10^t.test(a, b, var.equal = FALSE)$conf.int[1:2]))

cat("\n-- Hedges' g --\n")
# Worked by hand: a=(1,2,3) b=(4,5,6); s_pooled=1, d=-3, J=1-3/(4*4-1)=0.8
ok("g = d * (1 - 3/(4df-1))", eq(hedges_g(c(1, 2, 3), c(4, 5, 6)), -3 * (1 - 3 / 15)))
ok("g is smaller in magnitude than d", abs(hedges_g(c(1, 2, 3), c(4, 5, 6))) < 3)
ok("zero variance -> NA", is.na(hedges_g(c(2, 2, 2), c(2, 2, 2))))
ok("n<2 -> NA", is.na(hedges_g(2, c(3, 4, 5))))
ok("sign follows a - b", hedges_g(c(4, 5, 6), c(1, 2, 3)) > 0)

cat("\n-- Wilcoxon --\n")
wx <- run_groupwise_t_tests(dd, "sample", "none", "global", "0", "wilcoxon")
ok("uses the exact test when untied", eq(wx$p.value, wilcox.test(a, b, exact = TRUE)$p.value))
ok("exact p at n=3 v 3 cannot beat 0.05", min(wx$p.value) >= 0.1)
ok("estimate is Hodges-Lehmann, inside its own CI",
   wx$estimate >= wx$conf.low && wx$estimate <= wx$conf.high)
ok("small-n limitation is stated", any(grepl("smallest attainable", wx$message)))
tied <- mk(c(rep("A", 3), rep("B", 3)), rep(0, 6), rep(0, 6), rep(1:3, 2), 10^c(1, 1, 2, 1, 2, 2))
wt <- run_groupwise_t_tests(tied, "sample", "none", "global", "0", "wilcoxon")
ok("ties are disclosed", any(grepl("Ties present", wt$message)))
ok("test column names the rank test", grepl("Wilcoxon", wx$test[1]))

cat("\n-- multiplicity correction --\n")
set.seed(1)
many <- mk(rep(c("A", "B"), each = 18), rep(rep(c(0, 1, 2), each = 6), 2),
           rep(0, 36), rep(1:3, 12), 10^rnorm(36, 10, .3))
g <- run_groupwise_t_tests(many, "concentration_all", "BH", "global", "0", "welch")
ok("BH matches p.adjust over the whole table", eq(g$q.value, p.adjust(g$p.value, "BH")))
ok("q >= p always", all(g$q.value >= g$p.value - 1e-12, na.rm = TRUE))
gb <- run_groupwise_t_tests(many, "concentration_all", "bonferroni", "global", "0", "welch")
ok("Bonferroni matches p.adjust", eq(gb$q.value, p.adjust(gb$p.value, "bonferroni")))
gn <- run_groupwise_t_tests(many, "concentration_all", "none", "global", "0", "welch")
ok("no correction leaves q == p", eq(gn$q.value, gn$p.value))
gw <- run_groupwise_t_tests(many, "concentration_all", "BH", "within_panel", "0", "welch")
per_panel <- gw %>% group_by(panel) %>%
  summarise(okp = eq(q.value, p.adjust(p.value, "BH")), .groups = "drop")
ok("within-panel scope corrects inside each panel", all(per_panel$okp))

cat("\n-- significance thresholds --\n")
ok("star cutpoints", identical(
  as.character(significance_label(c(0.0009, 0.005, 0.04, 0.06))),
  c("***", "**", "*", "ns")))
ok("exactly 0.05 is ns", identical(as.character(significance_label(0.05)), "ns"))
ok("NA stays NA", is.na(significance_label(NA_real_)))
ok("stars read the ADJUSTED q, not the raw p",
   {sig <- run_groupwise_t_tests(many, "concentration_all", "bonferroni", "global", "0", "welch")
    identical(as.character(sig$significance), as.character(significance_label(sig$q.value)))})

cat("\n-- three-group coverage (the pair that used to be dropped) --\n")
d3 <- mk(rep(c("A", "B", "C"), each = 3), rep(0, 9), rep(0, 9), rep(1:3, 3),
         10^c(10.0, 10.2, 10.4, 9.5, 9.7, 9.9, 8.0, 8.2, 8.4))
s3 <- run_groupwise_t_tests(d3, "sample", "none", "global", "0", "welch")
ok("all 3 sample pairs are tested", nrow(s3) == 3, paste("rows:", nrow(s3)))
ok("contrasts are the 3 distinct pairs",
   setequal(s3$contrast, c("A - B", "A - C", "B - C")))
a3 <- annotation_data(s3, plot_summary(d3, "combined", "log10", "SD"), "sample",
                      "combined", "stars", TRUE, "log10")
ok("only the first pair is annotated on the plot", nrow(a3) == 1)
ok("annotated pair is A - B", identical(a3$contrast[1], "A - B"))

cat("\n-- ANOVA --\n")
unbal <- mk(c(rep("A", 3), rep("B", 2)), c(0, 0, 1, 0, 1), rep(0, 5), c(1, 2, 1, 1, 1),
            10^c(10, 10.2, 9.5, 9.9, 9.2))
av <- run_anova(unbal)
ok("ANOVA returns rows", nrow(av) > 0)
ok("SS type is declared", "ss_type" %in% names(av) && nzchar(av$ss_type[1]))
if (requireNamespace("car", quietly = TRUE)) {
  ok("unbalanced design uses Type II", grepl("Type II", av$ss_type[1]), av$ss_type[1])
}

cat("\n-- annotation offsets round-trip --\n")
base_ann <- annotation_data(s3, plot_summary(d3, "combined", "log10", "SD"), "sample",
                            "combined", "stars", TRUE, "log10")
key <- annotation_key(base_ann[1, ])
moved <- apply_annotation_offsets(base_ann, setNames(list(list(dx = 0.4, dy = 1.5)), key))
ok("offset shifts x", eq(moved$x_pos[1], base_ann$x_pos[1] + 0.4))
ok("offset shifts y", eq(moved$y[1], base_ann$y[1] + 1.5))
ok("unlisted labels are untouched",
   eq(apply_annotation_offsets(base_ann, list())$y, base_ann$y))

cat("\n-- bar keys --\n")
sm3 <- plot_summary(d3, "combined", "log10", "SD")
k <- bar_keys(sm3, "combined")
ok("one key per drawn bar", length(unique(k)) == nrow(sm3))
ok("key is sample then treatment", grepl("^A · ", k[1]))
ok("sample_time keys on treatment only",
   identical(bar_keys(plot_summary(d3, "sample_time", "log10", "SD"), "sample_time"),
             as.character(plot_summary(d3, "sample_time", "log10", "SD")$concentration_label)))
ok("input ids are syntactically valid",
   all(make.names(vapply(k, bar_color_input_id, character(1))) ==
       vapply(k, bar_color_input_id, character(1))))


# ---------------------------------------------------------------------------
# CPS-1: the unit of replication
# ---------------------------------------------------------------------------
# The defect: every unpaired test pulled every ROW in a group, so three
# biological replicates plated four times each entered as n = 12 on 22 df.
# The expected numbers below are re-derived by hand from the three biological
# means, never read back out of the app.
cat("\n-- the unit of replication (CPS-1) --\n")
bio_lab <- rep(paste0("R", 1:3), each = 4)          # 3 cultures, 4 plates each
plate_a <- c(7.90, 7.92, 7.88, 7.90,  8.00, 8.02, 7.98, 8.00,  8.10, 8.12, 8.08, 8.10)
plate_b <- plate_a + 0.4
dtech <- mk(rep(c("A", "B"), each = 12), rep(0, 24), rep(0, 24),
            rep(bio_lab, 2), 10^c(plate_a, plate_b))

ok("the biological replicate column is carried through prep",
   "bio_replicate" %in% names(dtech) && has_biological_replicates(dtech))

agg <- aggregate_biological_replicates(dtech)
ok("aggregation gives one row per biological replicate per group", nrow(agg) == 6,
   paste("rows", nrow(agg)))
ok("it counts the technical measurements it collapsed", all(agg$n_technical == 4))
# The collapsed value is the GEOMETRIC mean of the plates == arithmetic mean of log10.
ok("the collapsed value is the geometric mean of the plates",
   eq(sort(agg$log10_cfu[agg$sample == "A"]),
      sort(c(mean(plate_a[1:4]), mean(plate_a[5:8]), mean(plate_a[9:12])))))
ok("cfu is back-transformed consistently", eq(agg$cfu, 10^agg$log10_cfu))
ok("the biological replicate becomes the replicate label",
   identical(as.character(agg$replicate), as.character(agg$bio_replicate)))

r <- run_groupwise_t_tests(dtech, "sample", "BH", "global", "0", "welch")
# THE test the audit asked for: n reported must equal the biological replicates.
ok("n reported equals the biological replicates, not the wells",
   r$n_a == 3 && r$n_b == 3, paste("got", r$n_a, "and", r$n_b))
ok("the table names the unit of replication",
   identical(r$unit_of_replication[1], "biological replicate"))
hand <- t.test(c(mean(plate_a[1:4]), mean(plate_a[5:8]), mean(plate_a[9:12])),
               c(mean(plate_b[1:4]), mean(plate_b[5:8]), mean(plate_b[9:12])))
ok("p matches a hand Welch test on the three biological means",
   eq(r$p.value, unname(hand$p.value)))
ok("df matches too (not the 22 df of the pooled wells)",
   eq(r$parameter, unname(hand$parameter)))
ok("the wrong answer is genuinely different",
   !eq(r$p.value, t.test(plate_a, plate_b)$p.value))

# The ANOVA has to count the same units, or its residual df is inflated too.
av <- run_anova(dtech, "welch")
ok("the ANOVA is fitted to the aggregated units", av$n_units[1] == 6, paste(av$n_units[1]))
ok("the ANOVA names the unit of replication",
   identical(av$unit_of_replication[1], "biological replicate"))

# No declared biological replicate column: the app must NOT invent one, and must
# label what it did.
dnone <- mk(rep(c("A", "B"), each = 12), rep(0, 24), rep(0, 24),
            rep(bio_lab, 2), 10^c(plate_a, plate_b), bio = NA_character_)
ok("no biological column means no aggregation", !has_biological_replicates(dnone))
rn <- run_groupwise_t_tests(dnone, "sample", "BH", "global", "0", "welch")
ok("and n is the well count", rn$n_a == 12 && rn$n_b == 12)
ok("but the result is labelled technical replicates only",
   grepl("technical replicates only", rn$unit_of_replication[1], fixed = TRUE),
   rn$unit_of_replication[1])

# The summary and the figure caption must quote the same unit the test used.
sm_bio <- plot_summary(dtech, "combined", "log10", "SD")
ok("plot_summary counts biological replicates alongside the rows",
   all(sm_bio$n == 12) && all(sm_bio$n_bio == 3),
   paste("n:", paste(sm_bio$n, collapse = ","), "n_bio:", paste(sm_bio$n_bio, collapse = ",")))
ok("and reports NA for n_bio when none was declared",
   all(is.na(plot_summary(dnone, "combined", "log10", "SD")$n_bio)))

# The paired path already collapsed within a replicate label; the two paths now
# agree about what a replicate is.
dpair <- mk(rep(c("A", "B"), each = 12), rep(0, 24), rep(0, 24),
            rep(bio_lab, 2), 10^c(plate_a, plate_b))
rp <- run_groupwise_t_tests(dpair, "sample", "BH", "global", "0", "welch", paired = TRUE)
ok("the paired path matches the same three units", rp$n_matched == 3, paste(rp$n_matched))

# ---------------------------------------------------------------------------
# CPS-2: the non-default axis summarises on the scale it is drawn on
# ---------------------------------------------------------------------------
# The defect: "Raw CFU on log axis" summarised on the RAW scale, so the bar sat
# 2.35x above the geometric mean the test analysed and mean - SD went negative
# and was silently clamped to machine epsilon.
cat("\n-- the raw-CFU-on-log-axis summary (CPS-2) --\n")
vals <- c(1e6, 1e7, 1e8)                 # geometric mean exactly 1e7
dg <- mk(rep("A", 3), rep(0, 3), rep(0, 3), 1:3, vals)
raw <- plot_summary(dg, "sample_time", "raw_log_axis", "SD")
lgv <- log10(vals)
ok("the bar is the geometric mean, not the arithmetic mean", eq(raw$mean_y, 1e7))
ok("and the arithmetic mean really is different", !eq(raw$mean_y, mean(vals)))
ok("the interval is multiplicative: centre / 10^SD", eq(raw$ymin, 1e7 / 10^sd(lgv)))
ok("                              centre * 10^SD", eq(raw$ymax, 1e7 * 10^sd(lgv)))
ok("the lower whisker is strictly positive without clamping", raw$ymin > 1e-3)
ok("nothing lands on machine epsilon any more", !eq(raw$ymin, .Machine$double.eps))
ok("the multiplicative factor is reported", eq(raw$err_factor, 10^sd(lgv)))
ok("the summary says which scale it was taken on",
   identical(raw$summary_scale, "log10, back-transformed") &&
     identical(raw$centre_statistic, "geometric mean"))
# The back-transform must be an exact inverse of the log10 summary.
lg <- plot_summary(dg, "sample_time", "log10", "SD")
ok("it is exactly 10^ of the log10 summary",
   eq(raw$mean_y, 10^lg$mean_y) && eq(raw$ymin, 10^lg$ymin) && eq(raw$ymax, 10^lg$ymax))
ok("the log10 mode is unchanged", eq(lg$mean_y, mean(lgv)))

raw_ci <- plot_summary(dg, "sample_time", "raw_log_axis", "95% CI")
ok("a back-transformed 95% CI uses t(0.975, n-1) on the log scale",
   eq(raw_ci$ymax, 1e7 * 10^(qt(0.975, 2) * sd(lgv) / sqrt(3))))
raw_iqr <- plot_summary(dg, "sample_time", "raw_log_axis", "IQR")
# Quantiles are taken on the log scale and back-transformed, which is what makes
# the interval agree with the analysis; type-7 interpolation is not equivariant,
# so this is deliberately NOT the quantile of the raw counts.
ok("a back-transformed IQR is 10^ of the log-scale quantiles",
   eq(c(raw_iqr$ymin, raw_iqr$ymax), 10^unname(quantile(lgv, c(.25, .75), type = 7))))
ok("and that differs from the raw-scale quantile, as it must",
   !eq(raw_iqr$ymin, unname(quantile(vals, .25, type = 7))))

cap_raw <- error_type_caption("SD", "errorbar", "raw_log_axis")
ok("the caption names the geometric mean on the raw-log axis",
   grepl("geometric mean", cap_raw, fixed = TRUE), cap_raw)
ok("and says the spread is a log10 SD", grepl("10^SD of log10(CFU)", cap_raw, fixed = TRUE))
cap_log <- error_type_caption("SD", "errorbar", "log10")
ok("the log10 caption names the scale too",
   grepl("of log10(CFU)", cap_log, fixed = TRUE), cap_log)
ok("a survival caption is not relabelled",
   !grepl("log10(CFU)", error_type_caption("SD", "errorbar", "log10", is_survival = TRUE),
          fixed = TRUE))

# ---------------------------------------------------------------------------
# CPS-5 / CPS-7: the emmeans path
# ---------------------------------------------------------------------------
cat("\n-- the emmeans path (CPS-5, CPS-7) --\n")
set.seed(11)
dm <- mk(rep(c("A", "B", "C"), each = 9), rep(rep(c(0, 1, 2), each = 3), 3),
         rep(0, 27), rep(1:3, 9), 10^rnorm(27, 8, 0.25))
e_glob <- run_contrast(dm, "sample", "BH", "0", adjustment_scope = "global")
e_pan <- run_contrast(dm, "sample", "BH", "0", adjustment_scope = "within_panel")
ok("the emmeans frame now carries a panel column", "panel" %in% names(e_pan))
ok("one panel per level of the by-variable", n_distinct(e_pan$panel) == 3,
   paste(unique(e_pan$panel), collapse = " / "))
ok("the recorded scope is the scope the user asked for",
   identical(e_pan$adjustment_scope[1], "within_panel"))
# BH over 3 comparisons per panel is not BH over all 9: the control was inert
# before, so these two must differ.
ok("within-panel correction really differs from global",
   !isTRUE(all.equal(e_pan$q.value, e_glob$q.value)),
   paste(signif(e_pan$q.value, 3), collapse = ","))
ok("raw p values are untouched by the scope", eq(e_pan$p.value, e_glob$p.value))
ok("emmeans is fitted to the aggregated units",
   identical(e_glob$unit_of_replication[1], "biological replicate"))

e_tuk <- run_contrast(dm, "sample", "BH", "0", emm_adjust = "tukey")
ok("Tukey is labelled in the test column",
   grepl("Tukey", e_tuk$test[1], fixed = TRUE), e_tuk$test[1])
ok("and the app's own correction is disabled so nothing is corrected twice",
   identical(e_tuk$p_adjust_method[1], "none") && eq(e_tuk$q.value, e_tuk$p.value))
ok("Tukey p values are family-wise, so larger than the unadjusted ones",
   all(e_tuk$p.value >= e_glob$p.value - 1e-12))
e_dun <- run_contrast(dm, "concentration_vs_control", "BH", "0", emm_adjust = "dunnettx")
ok("Dunnett is available against the control",
   grepl("Dunnett", e_dun$test[1], fixed = TRUE), e_dun$test[1])
e_bad <- run_contrast(dm, "sample", "BH", "0", emm_adjust = "dunnettx")
ok("Dunnett on an all-pairs family degrades rather than mislabelling",
   identical(e_bad$emmeans_adjust[1], "none") &&
     grepl("single control", e_bad$message[1], fixed = TRUE))

ok("a rank-based pairwise family gets a rank-based omnibus",
   grepl("Kruskal-Wallis", run_anova(dm, "wilcoxon")$ss_type[1], fixed = TRUE))
ok("the parametric omnibus is still Type II",
   grepl("Type II", run_anova(dm, "welch")$ss_type[1], fixed = TRUE))
ok("the omnibus table says the t-test family is NOT its post-hoc",
   grepl("INDEPENDENT", run_anova(dm, "welch")$pairwise_family[1], fixed = TRUE))
ok("and says Tukey IS its post-hoc",
   grepl("post-hoc family", run_anova(dm, "emmeans_tukey")$pairwise_family[1], fixed = TRUE))


cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) {
  cat("ALL STATISTICS TESTS PASSED\n")
} else {
  cat(sprintf("%d TEST(S) FAILED\n", fails))
  quit(status = 1)
}
