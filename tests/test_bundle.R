# Bundle export test. Run from the project root:
#   Rscript tests/test_bundle.R [path/to/data.csv]
#
# Drives the real server, clicks the "Download everything" handler, and opens the
# archive it produces. Exits non-zero on failure.

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

app_lines <- readLines("app.R", warn = FALSE)
eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\n")),
     envir = globalenv())
app <- shiny::as.shiny.appobj(source("app.R", local = new.env())$value)

zipfile <- tempfile(fileext = ".zip")

# In testServer, ACCESSING a download output runs its handler and returns the
# path to the file it wrote.
shiny::testServer(app, {
  session$setInputs(file = list(datapath = normalizePath(csv), name = basename(csv)))
  cols <- names(raw_data())
  session$setInputs(col_sample = cols[1], col_conc = cols[2], col_time = cols[3],
                    col_rep = cols[4], col_cfu = cols[5])
  session$setInputs(
    stats_method = "welch", p_adjust = "BH", p_adjust_scope = "global",
    label_kind = "stars", show_ns = TRUE, show_points = TRUE,
    variation_display = "errorbar", bar_color_mode = "group", chart_geom = "point",
    y_mode = "log10", error_type = "SD", y_min = NA, y_max = NA,
    show_method_caption = TRUE, show_subtitle = FALSE, show_n_labels = TRUE,
    plot_theme = "classic", bar_orientation = "vertical", legend_position = "top",
    plot_title = "Bundle check", plot_subtitle = "", x_label = "Treatment",
    y_label = "CFU/mL", legend_title = "", treatment_unit = "", time_unit = "min",
    append_treatment_unit = FALSE, append_time_unit = TRUE,
    size_mode = "total", size_units = "in",
    download_width = 7, download_height = 4.4, download_dpi = 150,
    jitter_seed = 1, canvas_mode = "off", control_concentration = "0",
    ppt_editable = TRUE, bundle_gif = FALSE
  )
  d <- cfu_data()
  tl <- levels(d$time_min)

  # Survival mode, so the bundle has to carry the paired frame and summary.
  session$setInputs(plot_mode = "survival", samples = levels(d$sample),
                    surv_baseline = tl[1], surv_readout = tl[length(tl)],
                    surv_scale = "log10", comparison = "auto",
                    surv_match_replicates = FALSE)
  ok("survival frame is what will be bundled", is_survival_frame(plot_data()))

  produced <- output$download_all
  ok("the handler produced a file", is.character(produced) && file.exists(produced), paste(produced, collapse = ""))
  file.copy(produced, zipfile, overwrite = TRUE)
})

cat("\n-- the archive --\n")
ok("an archive was produced", file.exists(zipfile) && file.size(zipfile) > 20000,
   paste("bytes:", if (file.exists(zipfile)) file.size(zipfile) else 0))

listing <- zip::zip_list(zipfile)
names_in <- listing$filename
cat("     ", length(names_in), "files,", format(file.size(zipfile), big.mark = ","), "bytes\n")
for (n in sort(names_in)) cat(sprintf("       %-26s %s bytes\n", n, format(listing$compressed_size[listing$filename == n])))

expected <- c("figure.png", "figure.pdf", "figure.svg",
              "data_cleaned.csv", "data_plotted.csv", "summary.csv",
              "statistics.csv", "anova.csv", "qc_replicates.csv", "figure_qa.csv",
              "recreate_figure.R", "plot_preset.json", "analysis_manifest.json",
              "README.txt")
for (e in expected) ok(sprintf("contains %s", e), e %in% names_in)
ok("no file in the archive is empty", all(listing$size > 0),
   paste(names_in[listing$size == 0], collapse = ", "))
ok("the GIF was excluded when not requested", !("figure_reveal.gif" %in% names_in))

ex <- file.path(tempdir(), paste0("bundle_", as.integer(runif(1, 1, 1e9))))
dir.create(ex, recursive = TRUE, showWarnings = FALSE)
zip::unzip(zipfile, exdir = ex)

readme <- paste(readLines(file.path(ex, "README.txt"), warn = FALSE), collapse = "\n")
cat("\n-- the README --\n")
ok("names the source file", grepl(basename(csv), readme, fixed = TRUE))
ok("states the figure geometry", grepl("DPI", readme))
ok("says the readout is paired survival", grepl("paired survival", readme))
ok("lists what is inside", grepl("CONTENTS", readme))
ok("explains how to rebuild without the app", grepl("recreate_figure.R", readme, fixed = TRUE))

cat("\n-- the bundled artefacts are real --\n")
plotted <- read.csv(file.path(ex, "data_plotted.csv"), check.names = FALSE)
ok("the plotted data is the paired frame, not raw counts",
   "readout" %in% names(plotted) && all(plotted$readout == "survival_log10_ratio"))
ok("it carries both source counts for every ratio",
   all(c("baseline_log10", "readout_log10") %in% names(plotted)))
summ <- read.csv(file.path(ex, "summary.csv"), check.names = FALSE)
ok("the summary is the survival summary", "percent_survival" %in% names(summ))
stats <- read.csv(file.path(ex, "statistics.csv"), check.names = FALSE)
ok("the statistics carry pair counts", "n_pairs" %in% names(stats))

scr <- readLines(file.path(ex, "recreate_figure.R"), warn = FALSE)
ok("the script embeds the survival frame", any(grepl("survival_log10_ratio", scr, fixed = TRUE)))
ok("the script is self-contained (emits make_cfu_plot)",
   any(grepl("make_cfu_plot <- function", scr, fixed = TRUE)))
run <- tryCatch({
  old <- setwd(ex); on.exit(setwd(old), add = TRUE)
  source("recreate_figure.R", local = new.env()); "ran"
}, error = function(e) paste("ERROR:", conditionMessage(e)))
ok("the bundled script actually runs", identical(run, "ran"), run)
ok("and it wrote a figure", file.exists(file.path(ex, "cfu_plot_recreated.png")) &&
     file.size(file.path(ex, "cfu_plot_recreated.png")) > 10000)

pj <- jsonlite::read_json(file.path(ex, "plot_preset.json"))
ok("the preset records the plot mode", identical(pj$settings$plot_mode, "survival"))
mj <- jsonlite::read_json(file.path(ex, "analysis_manifest.json"))
ok("the manifest records the readout", identical(mj$visible_data$readout, "paired survival ratio"))

unlink(ex, recursive = TRUE); unlink(zipfile)
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL BUNDLE TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
