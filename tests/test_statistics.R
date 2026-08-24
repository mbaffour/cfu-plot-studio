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

mk <- function(sample, conc, time, rep, cfu) {
  raw <- tibble::tibble(Sample = sample, inducer_concentration = conc,
                        Time = time, Replicate = rep, CFU = cfu)
  prep_cfu_data(raw, list(sample = "Sample", concentration = "inducer_concentration",
                          time = "Time", replicate = "Replicate", cfu = "CFU"),
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

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) {
  cat("ALL STATISTICS TESTS PASSED\n")
} else {
  cat(sprintf("%d TEST(S) FAILED\n", fails))
  quit(status = 1)
}
