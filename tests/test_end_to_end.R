# End-to-end test: drives the REAL Shiny server with a real uploaded CSV.
# Run from the project root:  Rscript tests/test_end_to_end.R [path/to/data.csv]
#
# Everything else in tests/ exercises the helper functions. This one exercises
# the reactives, which is where a wrong frame reaching a consumer would hide.
# Exits non-zero on failure.

suppressPackageStartupMessages({library(shiny); library(ggplot2); library(dplyr); library(tidyr)})

args <- commandArgs(trailingOnly = TRUE)
csv <- if (length(args) > 0 && nzchar(args[1])) args[1] else {
  cand <- c("../gp75 cfu all reps_vault dummy.csv", "dummy_cfu_example.csv")
  cand[file.exists(cand)][1]
}
if (is.na(csv) || !file.exists(csv)) stop("No CSV to test with.", call. = FALSE)
cat("data file:", csv, "\n\n")

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}

# testServer evaluates its body in the server function's scope, which does not
# expose the app's top-level helpers by name. Load them into the global env so
# assertions can call them directly.
app_lines <- readLines("app.R", warn = FALSE)
eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\n")),
     envir = globalenv())

app <- shiny::as.shiny.appobj(source("app.R", local = new.env())$value)

shiny::testServer(app, {
  # --- upload -------------------------------------------------------------
  session$setInputs(file = list(datapath = normalizePath(csv), name = basename(csv)))
  cols <- names(raw_data())
  cat("-- upload and mapping --\n")
  ok("file parsed", length(cols) >= 5, paste(cols, collapse = ", "))

  session$setInputs(col_sample = cols[1], col_conc = cols[2], col_time = cols[3],
                    col_rep = cols[4], col_cfu = cols[5])

  # The UI supplies these on a real session; testServer starts them all NULL.
  # They must be set BEFORE reading the factor levels: append_time_unit changes
  # the timepoint labels, so levels captured first would no longer match.
  session$setInputs(
    stats_method = "welch", p_adjust = "BH", p_adjust_scope = "global",
    label_kind = "stars", show_ns = TRUE, show_points = TRUE,
    variation_display = "errorbar", bar_color_mode = "group", chart_geom = "bar",
    y_mode = "log10", error_type = "SD", y_min = 0, y_max = NA,
    show_method_caption = TRUE, show_subtitle = FALSE, show_n_labels = TRUE,
    plot_theme = "classic", bar_orientation = "vertical", legend_position = "top",
    plot_title = "", plot_subtitle = "", x_label = "Treatment", y_label = "CFU/mL",
    legend_title = "", treatment_unit = "", time_unit = "min",
    append_treatment_unit = FALSE, append_time_unit = TRUE,
    size_units = "in", download_width = 7, download_height = 4.4, download_dpi = 600,
    jitter_seed = 1, canvas_mode = "off", control_concentration = "0"
  )

  d <- cfu_data()
  ok("rows survive prep", nrow(d) > 0, paste("rows:", nrow(d)))
  ok("log10_cfu is finite everywhere", all(is.finite(d$log10_cfu)))
  drop <- dropped_rows()
  ok("dropped rows are accounted for", drop$lost == drop$total - nrow(d),
     paste("lost", drop$lost, "of", drop$total, "kept", nrow(d)))

  tl <- levels(d$time_min)
  ok("at least two timepoints, so survival is possible", length(tl) >= 2,
     paste(tl, collapse = ", "))

  # --- absolute mode still works -----------------------------------------
  cat("\n-- absolute CFU mode --\n")
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = tl,
                    comparison = "auto")
  ok("plot_data is the raw frame here", !is_survival_frame(plot_data()))
  ok("summary has rows", nrow(current_summary()) > 0)
  ok("stats run", nrow(current_stats()) > 0)
  ok("the figure builds", inherits(current_plot(), "ggplot"))
  ok("no pairing QC in absolute mode", is.null(survival_qc()))

  # --- survival mode ------------------------------------------------------
  cat("\n-- paired survival mode --\n")
  session$setInputs(plot_mode = "survival", samples = levels(d$sample),
                    surv_baseline = tl[1], surv_readout = tl[length(tl)],
                    surv_scale = "log10", comparison = "auto")
  sv <- plot_data()
  ok("plot_data switches to the paired frame", is_survival_frame(sv))
  ok("every row is a complete pair", nrow(sv) > 0 && all(is.finite(sv$log10_cfu)))
  ok("the ratio is readout minus baseline",
     isTRUE(all.equal(sv$log10_cfu, sv$readout_log10 - sv$baseline_log10)))

  q <- survival_qc()
  ok("pairing QC is populated", !is.null(q) && isTRUE(q$ok),
     paste("complete:", q$complete))
  cat(sprintf("     %d complete pairs, %d orphans, %d unlabelled, empty cells: %s\n",
              q$complete, q$baseline_only + q$readout_only, q$unlabelled,
              if (length(q$empty_cells)) paste(q$empty_cells, collapse = "; ") else "none"))

  ok("auto comparison resolves to the paired test",
     identical(active_comparison(), "survival_vs_zero"))
  st <- current_stats()
  ok("survival stats produce rows", nrow(st) > 0)
  ok("they report pair counts", "n_pairs" %in% names(st))
  ok("they name the paired effect size",
     "effect_size_kind" %in% names(st) && any(st$effect_size_kind == "paired (d_z)"))
  ok("no NaN reaches the p column", !any(is.nan(st$p.value)))
  ok("the survival figure builds", inherits(current_plot(), "ggplot"))

  b <- ggplot_build(current_plot())
  hl <- Filter(function(z) "yintercept" %in% names(z), b$data)
  ok("a no-change reference line is drawn at 0",
     length(hl) > 0 && isTRUE(all.equal(unique(hl[[1]]$yintercept), 0)))
  sm <- current_summary()
  ok("negative error bars are not clamped away",
     all(sm$ymin <= sm$ymax, na.rm = TRUE))

  # --- every download in survival mode -----------------------------------
  cat("\n-- downloads in survival mode --\n")
  se <- summary_for_export()
  ok("Summary CSV exports the survival table, not raw CFU",
     "n_pairs" %in% names(se) && "percent_survival" %in% names(se),
     paste(names(se), collapse = ", "))
  ok("Summary CSV has one row per cell, not per timepoint",
     nrow(se) == nrow(distinct(sv, sample, concentration_label)))

  scr <- reproducible_script()
  ok("the R script export embeds the PAIRED frame",
     grepl("survival_log10_ratio", scr, fixed = TRUE))
  ok("it emits the survival helpers it calls",
     grepl("survival_axis_label", scr, fixed = TRUE) &&
     grepl("survival_scale_labeller", scr, fixed = TRUE) &&
     grepl("resolve_auto_comparison", scr, fixed = TRUE))

  mf <- manifest_payload()
  ok("the manifest records this as a survival readout",
     identical(mf$visible_data$readout, "paired survival ratio"))
  ok("the manifest carries the pairing audit",
     !is.null(mf$visible_data$survival_pairing$complete))

  qa <- figure_qa()
  ok("Figure QA includes the pairing check", any(grepl("Pairing", qa$check)))

  # --- the refusals -------------------------------------------------------
  cat("\n-- refusals --\n")
  session$setInputs(comparison = "time")
  ok("the timepoint test is refused with a reason",
     "message" %in% names(current_stats()) &&
       grepl("already been consumed", current_stats()$message[1]))
  ok("the figure still builds while refused", inherits(current_plot(), "ggplot"))

  session$setInputs(plot_mode = "combined", comparison = "survival_vs_zero",
                    samples = levels(d$sample), times = tl)
  ok("the survival test is refused outside survival mode",
     "message" %in% names(current_stats()))
  cp <- ggplot_build(current_plot())$plot$labels$caption
  ok("and the caption does not claim it ran",
     is.null(cp) || !grepl("survival ratio against no change", cp), cp %||% "")

  # --- back to survival, exports render -----------------------------------
  cat("\n-- figure export renders --\n")
  session$setInputs(plot_mode = "survival", comparison = "auto",
                    surv_baseline = tl[1], surv_readout = tl[length(tl)])
  tmp <- tempfile(fileext = ".png")
  r <- tryCatch({
    ggsave(tmp, current_plot(), width = 7, height = 4.4, units = "in", dpi = 150); "ok"
  }, error = function(e) conditionMessage(e))
  ok("a survival PNG renders", identical(r, "ok") && file.exists(tmp) && file.size(tmp) > 5000, r)
})

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL END-TO-END TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
