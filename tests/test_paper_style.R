# Paper style test (0.4.0). Run from the project root:
#   Rscript tests/test_paper_style.R
#
# One style for every figure of a paper: colours keyed by sample / timepoint
# NAME, a font family threaded through every text the figure draws, and a
# paper_style.json applied at startup. Drives the real server with
# shiny::testServer, writes files through the real download handlers, and runs
# the bundled recreate_figure.R in a separate R process. Test 1 renders the
# default figure with the 0.3.0 code (commit 0762ca1) and with this code, in
# separate R processes, and compares the SVG files byte for byte.
# Exits non-zero on failure.

suppressPackageStartupMessages({library(shiny); library(ggplot2); library(dplyr); library(tidyr)})

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}
note <- function(label, detail) cat(sprintf("  PASS  %s   (note: %s)\n", label, detail))

work <- file.path(tempdir(), paste0("paperstyle_", as.integer(runif(1, 1, 1e9))))
dir.create(work, recursive = TRUE, showWarnings = FALSE)
# Never touch a paper style saved beside app.R; Test 5 points this elsewhere.
no_style <- file.path(work, "no_paper_style.json")
Sys.setenv(CFU_PAPER_STYLE = no_style)

rscript <- file.path(R.home("bin"), "Rscript")

# ===========================================================================
# 1. Byte-identical defaults against 0.3.0
# ===========================================================================
cat("\n-- 1. default figures are byte-identical to 0.3.0 --\n")

# The child renders the default figure (every input at the value its UI gives
# it, demo data) in each plot mode and writes the SVG through the app's own
# download handler. Run once per app.R, each in a fresh R, so the two
# definitions of every helper never meet.
child <- file.path(work, "render_defaults.R")
writeLines(c(
  'args <- commandArgs(TRUE); app_path <- args[1]; out_dir <- args[2]',
  'Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style.json"))',
  'suppressPackageStartupMessages(library(shiny))',
  'app_lines <- readLines(app_path, warn = FALSE)',
  'eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\\n")), envir = globalenv())',
  'env <- new.env()',
  'app <- shiny::as.shiny.appobj(source(app_path, local = env)$value)',
  '# Every static input at its UI default, read off the rendered page.',
  'ui_defaults <- function(ui) {',
  '  doc <- xml2::read_html(as.character(htmltools::renderTags(ui)$html))',
  '  out <- list()',
  '  for (n in xml2::xml_find_all(doc, "//input[@id]")) {',
  '    id <- xml2::xml_attr(n, "id"); type <- xml2::xml_attr(n, "type"); cls <- xml2::xml_attr(n, "class")',
  '    type <- if (is.na(type)) "" else type; cls <- if (is.na(cls)) "" else cls',
  '    if (type %in% c("file", "button", "submit", "hidden", "radio")) next',
  '    val <- if (grepl("js-range-slider", cls)) as.numeric(xml2::xml_attr(n, "data-from"))',
  '      else if (type == "checkbox") !is.na(xml2::xml_attr(n, "checked"))',
  '      else if (grepl("shiny-colour-input", cls)) xml2::xml_attr(n, "data-init-value")',
  '      else if (type == "number") suppressWarnings(as.numeric(xml2::xml_attr(n, "value")))',
  '      else { v <- xml2::xml_attr(n, "value"); if (is.na(v)) "" else v }',
  '    out[[id]] <- val',
  '  }',
  '  for (n in xml2::xml_find_all(doc, "//select[@id]")) {',
  '    opts <- xml2::xml_find_all(n, ".//option")',
  '    sel <- opts[!is.na(xml2::xml_attr(opts, "selected"))]',
  '    out[[xml2::xml_attr(n, "id")]] <- xml2::xml_attr(if (length(sel) > 0) sel[[1]] else opts[[1]], "value")',
  '  }',
  '  for (g in xml2::xml_find_all(doc, "//div[contains(@class, \'shiny-input-radiogroup\')]")) {',
  '    r <- xml2::xml_find_all(g, ".//input[@type=\'radio\']")',
  '    chk <- r[!is.na(xml2::xml_attr(r, "checked"))]',
  '    out[[xml2::xml_attr(g, "id")]] <- xml2::xml_attr(if (length(chk) > 0) chk[[1]] else r[[1]], "value")',
  '  }',
  '  out',
  '}',
  'defaults <- ui_defaults(env$ui)',
  'writeLines(sort(names(defaults)), file.path(out_dir, "input_ids.txt"))',
  'shiny::testServer(app, {',
  '  do.call(session$setInputs, defaults)',
  '  # Inputs a renderUI creates, at the values that UI gives them.',
  '  session$setInputs(col_sample = "Group", col_conc = "Treatment", col_time = "Timepoint",',
  '                    col_rep = "Replicate", col_bio_rep = "__same__", col_cfu = "CFU")',
  '  d <- cfu_data()',
  '  sl <- levels(d$sample); tl <- levels(d$time_min)',
  '  session$setInputs(control_concentration = levels(d$concentration_label)[1])',
  '  modes <- list(',
  '    combined = list(plot_mode = "combined", samples = sl, times = tl, y_min = 0, y_max = NA),',
  '    sample_both = list(plot_mode = "sample_both", single_sample = sl[1], times = tl, y_min = 0, y_max = NA),',
  '    sample_time = list(plot_mode = "sample_time", single_sample = sl[1], single_time = tl[1], y_min = 0, y_max = NA),',
  '    survival = list(plot_mode = "survival", samples = sl, surv_scale = "log10",',
  '                    surv_baseline = survival_time_levels(d)[1],',
  '                    surv_readout = rev(survival_time_levels(d))[1],',
  '                    surv_match_replicates = FALSE, y_min = NA, y_max = NA)',
  '  )',
  '  for (m in names(modes)) {',
  '    do.call(session$setInputs, modes[[m]])',
  '    file.copy(output$download_svg, file.path(out_dir, paste0(m, ".svg")), overwrite = TRUE)',
  '    k <- tryCatch(output$download_key_svg, error = function(e) NULL)',
  '    if (!is.null(k)) file.copy(k, file.path(out_dir, paste0("key_", m, ".svg")), overwrite = TRUE)',
  '  }',
  '})'
), child)

render_with <- function(app_path, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  res <- suppressWarnings(system2(rscript, c(shQuote(child), shQuote(app_path), shQuote(out_dir)),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(res, "status") %||% 0L, log = res)
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

base_app <- file.path(work, "app_base.R")
# The baseline is pinned to the 0.3.0 commit, not a branch name, so it survives
# the branch being merged and deleted. A clone without that history (a shallow
# clone, a release archive) cannot run this comparison and says so, as
# test_survival.R does for its private dataset, rather than failing.
BASE_COMMIT <- "0762ca1"
git <- tryCatch(suppressWarnings(system2("git", c("show", paste0(BASE_COMMIT, ":app.R")), stdout = base_app, stderr = FALSE)),
                error = function(e) 1L)
have_base <- identical(as.integer(git), 0L) && file.exists(base_app) && file.size(base_app) > 0
if (have_base) {
  ok("the 0.3.0 app.R is available from commit 0762ca1", TRUE)
} else {
  cat("  PASS  (skipped) commit 0762ca1 is not in this clone, so the byte-identical comparison with 0.3.0 cannot run here\n")
}

if (have_base) {
  old_dir <- file.path(work, "old"); new_dir <- file.path(work, "new"); new2_dir <- file.path(work, "new2")
  r_old <- render_with(base_app, old_dir)
  r_new <- render_with(normalizePath("app.R"), new_dir)
  r_new2 <- render_with(normalizePath("app.R"), new2_dir)
  ok("0.3.0 renders its default figures", identical(as.integer(r_old$status), 0L), paste(tail(r_old$log, 5), collapse = "\n"))
  ok("0.4.0 renders its default figures", identical(as.integer(r_new$status), 0L), paste(tail(r_new$log, 5), collapse = "\n"))
  ids_old <- readLines(file.path(old_dir, "input_ids.txt")); ids_new <- readLines(file.path(new_dir, "input_ids.txt"))
  ok("the defaults differ only by the colour pickers and the font controls",
     setequal(setdiff(ids_old, ids_new), c("sample_color_1", "sample_color_2", "time_color_1", "time_color_2")) &&
       setequal(setdiff(ids_new, ids_old), c("font_family", "font_family_custom")),
     paste("old only:", paste(setdiff(ids_old, ids_new), collapse = ","), "new only:", paste(setdiff(ids_new, ids_old), collapse = ",")))
  files <- list.files(old_dir, pattern = "\\.svg$")
  ok("all four modes rendered, figure and key", all(c("combined.svg", "sample_both.svg", "sample_time.svg", "survival.svg") %in% files),
     paste(files, collapse = ", "))
  same_bytes <- function(a, b) file.exists(a) && file.exists(b) &&
    identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
  # svglite output is deterministic run to run; prove it before trusting a
  # byte comparison, and only normalise if it is not.
  deterministic <- all(vapply(files, function(f) same_bytes(file.path(new_dir, f), file.path(new2_dir, f)), logical(1)))
  ok("the same code renders byte-identical SVG twice (nothing to normalise)", deterministic)
  for (f in files) {
    ok(sprintf("%s is byte-identical to 0.3.0", f), same_bytes(file.path(old_dir, f), file.path(new_dir, f)),
       sprintf("%s vs %s bytes", file.size(file.path(old_dir, f)), file.size(file.path(new_dir, f))))
  }
}

# ===========================================================================
# Shared fixtures and readers (as in test_legend_key.R)
# ===========================================================================
app_lines <- readLines("app.R", warn = FALSE)
eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\n")),
     envir = globalenv())
app <- shiny::as.shiny.appobj(source("app.R", local = new.env())$value)

fixture <- function(names) {
  grid <- expand.grid(Replicate = 1:3, Timepoint = c("Early", "Late"),
                      Treatment = c("Baseline", "Treatment A"), Group = names,
                      stringsAsFactors = FALSE)
  set.seed(7)
  grid$CFU <- round(10^(6 + as.integer(factor(grid$Group, levels = names)) * 0.3 +
                          (grid$Timepoint == "Late") * 0.5 + rnorm(nrow(grid), 0, 0.1)))
  f <- tempfile(fileext = ".csv", tmpdir = work)
  write.csv(grid[, c("Group", "Treatment", "Timepoint", "Replicate", "CFU")], f, row.names = FALSE)
  f
}

norm_hex <- function(x) toupper(grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue = 255))

svg_parts <- function(path) {
  doc <- xml2::read_xml(path)
  xml2::xml_ns_strip(doc)
  texts <- xml2::xml_find_all(doc, ".//text")
  style_val <- function(nodes, key) {
    st <- xml2::xml_attr(nodes, "style")
    m <- regmatches(st, regexec(paste0(key, ":\\s*([^;]+);"), st))
    vapply(m, function(z) if (length(z) > 1) trimws(z[2]) else NA_character_, character(1))
  }
  glyphs <- xml2::xml_find_all(doc, ".//circle | .//rect")
  num <- function(x) suppressWarnings(as.numeric(x))
  list(
    text = xml2::xml_text(texts),
    text_y = as.numeric(xml2::xml_attr(texts, "y")),
    text_x = as.numeric(xml2::xml_attr(texts, "x")),
    text_font = gsub('"', "", style_val(texts, "font-family"), fixed = TRUE),
    glyph_fill = toupper(style_val(glyphs, "fill")),
    glyph_stroke = toupper(style_val(glyphs, "stroke")),
    glyph_x = vapply(seq_along(glyphs), function(i) {
      g <- glyphs[[i]]
      if (xml2::xml_name(g) == "circle") num(xml2::xml_attr(g, "cx"))
      else num(xml2::xml_attr(g, "x")) + num(xml2::xml_attr(g, "width")) / 2
    }, numeric(1)),
    glyph_y = vapply(seq_along(glyphs), function(i) {
      g <- glyphs[[i]]
      if (xml2::xml_name(g) == "circle") num(xml2::xml_attr(g, "cy"))
      else num(xml2::xml_attr(g, "y")) + num(xml2::xml_attr(g, "height")) / 2
    }, numeric(1))
  )
}

row_fill <- function(sv, label) {
  i <- match(label, sv$text)
  if (is.na(i)) return(NA_character_)
  coloured <- which(!is.na(sv$glyph_stroke) & sv$glyph_stroke != "NONE" &
                      sv$glyph_x < sv$text_x[i] & sv$glyph_x > sv$text_x[i] - 30)
  if (length(coloured) == 0) return(NA_character_)
  j <- coloured[which.min(abs(sv$glyph_y[coloured] - sv$text_y[i]))]
  sv$glyph_fill[j]
}

# The real panel rectangle of a drawn gtable, in inches (test_panel_size.R).
measured_panel <- function(g, fig_w = 14, fig_h = 12) {
  f <- tempfile(fileext = ".png")
  png(f, width = fig_w, height = fig_h, units = "in", res = 150)
  on.exit({dev.off(); unlink(f)}, add = TRUE)
  grid::grid.newpage(); grid::grid.draw(g)
  lay <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
  c(w = grid::convertWidth(g$widths[unique(lay$l)][1], "in", valueOnly = TRUE),
    h = grid::convertHeight(g$heights[unique(lay$t)][1], "in", valueOnly = TRUE))
}

base_inputs <- list(
  stats_method = "welch", p_adjust = "BH", p_adjust_scope = "global", label_kind = "stars",
  show_ns = TRUE, show_points = TRUE, variation_display = "errorbar", bar_color_mode = "group",
  chart_geom = "point", y_mode = "log10", error_type = "SD", y_min = NA, y_max = NA,
  show_method_caption = TRUE, show_subtitle = FALSE, show_n_labels = TRUE,
  plot_theme = "classic", bar_orientation = "vertical", legend_position = "right",
  plot_title = "Paper style check", plot_subtitle = "", x_label = "Treatment",
  y_label = "CFU/mL", legend_title = "", treatment_unit = "", time_unit = "min",
  append_treatment_unit = FALSE, append_time_unit = TRUE,
  size_mode = "total", size_units = "in", download_width = 7, download_height = 4.4,
  download_dpi = 150, jitter_seed = 1, canvas_mode = "off", comparison = "sample",
  ppt_editable = TRUE, bundle_gif = FALSE, font_size = 11, font_family = "sans",
  font_family_custom = "", single_color = "#7AA6C2",
  bar_outline_color = "#262626", bar_outline_width = 0.25, legend_key_on_figure = FALSE
)

# Inside testServer: load a CSV through the file input and map its columns.
load_csv_in <- function(session, raw_data, path) {
  session$setInputs(file = list(datapath = normalizePath(path), name = basename(path)))
  cols <- names(raw_data())
  session$setInputs(col_sample = cols[1], col_conc = cols[2], col_time = cols[3],
                    col_rep = cols[4], col_cfu = cols[5])
}

cat("\n-- the setting lists restore every new control --\n")
ok("font_family is a plot setting restored as a select",
   "font_family" %in% plot_setting_ids && "font_family" %in% select_setting_ids)
ok("font_family_custom is a plot setting restored as text",
   "font_family_custom" %in% plot_setting_ids && "font_family_custom" %in% text_setting_ids)
ok("no colour picker is restored through updateSliderInput",
   !any(c(colour_setting_ids, "font_family", "font_family_custom") %in% slider_setting_ids))
ok("the numbered colour ids are gone from the plot settings",
   !any(c("sample_color_1", "sample_color_2", "time_color_1", "time_color_2") %in% plot_setting_ids))
ok("a style skips exactly the seven figure-specific settings",
   setequal(unname(figure_specific_setting_ids),
            c("plot_title", "plot_mode", "y_min", "y_max", "surv_baseline", "surv_readout", "comparison")))

csv_abc <- fixture(c("A", "B", "C"))
csv_cda <- fixture(c("C", "D", "A"))
csv_be <- fixture(c("B", "E"))
# Unequal n, so the n row is drawn under the axis (it is left off when every
# group has the same n).
csv_uneven <- local({
  f <- fixture(c("A", "B", "C")); x <- read.csv(f, check.names = FALSE)
  write.csv(x[-1, ], f, row.names = FALSE); f
})
chosen_b <- "#8E44AD"
preset_path <- file.path(work, "preset.json")

# ===========================================================================
# 2. Colour by name
# ===========================================================================
cat("\n-- 2. colours follow the sample NAME across CSVs --\n")
shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_abc)
  do.call(session$setInputs, base_inputs)
  show_all <- function() {
    d <- cfu_data()
    session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                      control_concentration = levels(d$concentration_label)[1])
  }
  show_all()
  r1 <- colour_registry()$samples
  ok("A, B, C take the defaults in level order",
     identical(unname(r1[c("A", "B", "C")]), c("#0072B2", "#D55E00", "#009E73")), paste(r1, collapse = ","))
  m1 <- key_mapping()$entries
  ok("the figure draws the registry colours",
     identical(setNames(m1$fill, m1$name)[c("A", "B", "C")], r1[c("A", "B", "C")]))

  load_csv_in(session, raw_data, csv_cda)
  show_all()
  r2 <- colour_registry()$samples
  ok("C keeps its colour in the next CSV", identical(r2[["C"]], r1[["C"]]), r2[["C"]])
  ok("A keeps its colour in the next CSV", identical(r2[["A"]], r1[["A"]]), r2[["A"]])
  ok("D, a new name, takes the next unused default (#CC79A7)", identical(r2[["D"]], "#CC79A7"), r2[["D"]])
  ok("B is remembered though this CSV lacks it", identical(r2[["B"]], r1[["B"]]))
  m2 <- key_mapping()$entries
  ok("the figure of the second CSV draws C, D, A in their registry colours",
     identical(setNames(m2$fill, m2$name)[c("C", "D", "A")], r2[c("C", "D", "A")]))
  ok("the sample pickers are one per loaded name",
     all(c(sample_colour_input_id("C"), sample_colour_input_id("D"), sample_colour_input_id("A")) %in%
           unlist(regmatches(output$sample_colour_ui$html, gregexpr("samplecol_[A-Za-z0-9._]+", output$sample_colour_ui$html)))) &&
       !grepl(sample_colour_input_id("B"), output$sample_colour_ui$html, fixed = TRUE))

  load_csv_in(session, raw_data, csv_abc)
  show_all()
  do.call(session$setInputs, setNames(list(chosen_b), sample_colour_input_id("B")))
  ok("recolouring B through its picker reaches the registry", identical(colour_registry()$samples[["B"]], chosen_b))
  file.copy(output$download_preset, preset_path, overwrite = TRUE)
  # Okabe-Ito goes to the names loaded, in level order; D (not loaded) keeps its colour.
  keep_d <- colour_registry()$samples[["D"]]
  saved <- colour_registry()
  session$setInputs(apply_okabe_ito = 1)
  ok("Apply Okabe-Ito colours the loaded samples in level order",
     identical(unname(colour_registry()$samples[levels(cfu_data()$sample)]), okabe_ito[1:3]))
  ok("Apply Okabe-Ito colours the loaded timepoints in level order",
     identical(unname(colour_registry()$times[levels(cfu_data()$time_min)]), okabe_ito[1:2]))
  ok("Apply Okabe-Ito leaves names not loaded alone", identical(colour_registry()$samples[["D"]], keep_d))
  colour_registry(saved)
})
pj <- jsonlite::read_json(preset_path, simplifyVector = TRUE)
ok("the preset carries sample_palette by name", identical(pj$settings$sample_palette$B, chosen_b) &&
     all(c("A", "B", "C", "D") %in% names(pj$settings$sample_palette)), paste(names(pj$settings$sample_palette), collapse = ","))
ok("the preset carries time_palette by name", all(c("Early min", "Late min") %in% names(pj$settings$time_palette)) ||
     all(c("Early", "Late") %in% names(pj$settings$time_palette)), paste(names(pj$settings$time_palette), collapse = ","))
ok("the preset no longer carries numbered colours",
   !any(c("sample_color_1", "sample_color_2", "time_color_1", "time_color_2") %in% names(pj$settings)))

shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_be)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])
  ok("a fresh session gives B the first default", identical(colour_registry()$samples[["B"]], "#0072B2"))
  session$setInputs(preset_file = list(datapath = preset_path, name = "preset.json"))
  reg <- colour_registry()$samples
  ok("after loading the preset B has the chosen colour", identical(reg[["B"]], chosen_b), reg[["B"]])
  ok("names the CSV lacks are kept from the preset", all(c("A", "C", "D") %in% names(reg)))
  ok("E keeps the colour it was given", identical(reg[["E"]], "#D55E00"), reg[["E"]])
  sv <- svg_parts(output$download_key_svg)
  ok("the legend key SVG draws B in the chosen colour", identical(row_fill(sv, "B"), norm_hex(chosen_b)), row_fill(sv, "B"))
  ok("the legend key SVG draws E in its registry colour", identical(row_fill(sv, "E"), norm_hex(reg[["E"]])), row_fill(sv, "E"))
  txt <- legend_text()
  word_b <- colour_words(c(chosen_b, reg[["E"]]))[1]
  ok("the text starter names B's chosen colour", grepl(paste0("B (", word_b, ","), txt, fixed = TRUE), txt)
  # the figure itself
  built <- ggplot2::ggplot_build(current_plot())
  drawn <- unique(norm_hex(unlist(lapply(built$data, function(l) l$fill[!is.na(l$fill)]))))
  ok("the figure draws B in the chosen colour", norm_hex(chosen_b) %in% drawn, paste(drawn, collapse = ","))
})

# ===========================================================================
# 3. Old presets
# ===========================================================================
cat("\n-- 3. a preset from before 0.4.0 --\n")
old_preset <- file.path(work, "old_preset.json")
writeLines('{"app": "CFU Plot Studio", "preset_version": 1, "settings": {"sample_color_1": "#111111", "sample_color_2": "#222222", "font_size": 12}}', old_preset)
shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_abc)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])
  session$setInputs(preset_file = list(datapath = old_preset, name = "old_preset.json"))
  reg <- colour_registry()$samples
  lv <- levels(d$sample)
  ok("sample_color_1 colours the first loaded sample", identical(reg[[lv[1]]], "#111111"), reg[[lv[1]]])
  ok("sample_color_2 colours the second loaded sample", identical(reg[[lv[2]]], "#222222"), reg[[lv[2]]])
  ok("the third sample keeps its colour", identical(reg[[lv[3]]], "#009E73"), reg[[lv[3]]])
  m <- key_mapping()$entries
  ok("the figure draws them so", identical(unname(setNames(m$fill, m$name)[lv[1:2]]), c("#111111", "#222222")))
  ok("the rest of the old preset still applies", identical(last_applied_settings()$font_size, 12L) ||
       identical(as.numeric(last_applied_settings()$font_size), 12))
})

# ===========================================================================
# 4. Font
# ===========================================================================
cat("\n-- 4. the font family reaches every text --\n")
fonts_to_check <- c("Arial", "Times New Roman")
# Plotmath symbols (the log10 axis title) are drawn by R in the Symbol face
# whatever the family, so those glyph nodes are not the figure's font to set.
not_symbol <- function(sv) sv$text_font[sv$text_font != "Symbol"]
shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_uneven)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])
  default_font <- unique(not_symbol(svg_parts(output$download_svg)))

  for (fam in fonts_to_check) {
    if (!isTRUE(resolve_font_family(fam)$available)) {
      note(sprintf("%s: skipped", fam), sprintf("%s is not installed on this machine", fam))
      next
    }
    if (identical(fam, "Arial") && identical(default_font, "Arial")) {
      cat("     (the default sans is already drawn as Arial here, so Times New Roman is what proves the threading)\n")
    }
    for (theme in c("classic", "minimal_grid")) {
      session$setInputs(font_family = fam, plot_theme = theme, size_mode = "total")
      fv <- svg_parts(output$download_svg)
      ann <- current_annotation()
      ok(sprintf("%s / %s: every text node of the figure SVG is %s", fam, theme, fam),
         length(not_symbol(fv)) > 0 && all(not_symbol(fv) == fam), paste(unique(fv$text_font), collapse = ","))
      ok(sprintf("%s / %s: axis text is there (Baseline)", fam, theme), "Baseline" %in% fv$text)
      ok(sprintf("%s / %s: stat labels are there", fam, theme), nrow(ann) > 0 && all(ann$label %in% fv$text),
         paste(ann$label, collapse = ","))
      ok(sprintf("%s / %s: n labels are there", fam, theme), all(c("2", "3") %in% fv$text))
      kv <- svg_parts(output$download_key_svg)
      ok(sprintf("%s / %s: the legend key is %s", fam, theme, fam), all(c("A", "B", "C") %in% kv$text) &&
           all(kv$text_font == fam), paste(unique(kv$text_font), collapse = ","))
    }
    session$setInputs(plot_theme = "classic", size_mode = "panel", size_units = "in",
                      download_width = 4, download_height = 2.4)
    g <- current_grob()
    m <- measured_panel(g)
    ok(sprintf("%s: the pinned panel still measures 4.000 x 2.400 in", fam),
       abs(m[["w"]] - 4) < 1e-4 && abs(m[["h"]] - 2.4) < 1e-4, sprintf("%.4f x %.4f", m[["w"]], m[["h"]]))
    session$setInputs(size_mode = "total", download_width = 7, download_height = 4.4)
  }

  for (how in c("custom", "direct")) {
    if (how == "custom") session$setInputs(font_family = "custom", font_family_custom = "NoSuchFont-XYZ")
    else session$setInputs(font_family = "NoSuchFont-XYZ", font_family_custom = "")
    path <- tryCatch(output$download_svg, error = function(e) e)
    ok(sprintf("unknown font (%s): the figure renders without error", how), is.character(path) && file.exists(path),
       if (inherits(path, "error")) conditionMessage(path) else "")
    if (is.character(path)) {
      fv <- svg_parts(path)
      ok(sprintf("unknown font (%s): drawn in the default sans", how), all(not_symbol(fv) == default_font),
         paste(unique(fv$text_font), collapse = ","))
    }
    qa <- figure_qa()
    row <- qa[qa$check == "Font", ]
    ok(sprintf("unknown font (%s): Figure QA carries the fallback row", how),
       nrow(row) == 1 && row$status == "Review" && grepl("NoSuchFont-XYZ", row$details, fixed = TRUE) &&
         grepl("drawn in sans", row$details, fixed = TRUE), paste(row$details, collapse = " "))
  }
  session$setInputs(font_family = "sans", font_family_custom = "")
  qa <- figure_qa()
  ok("default font: Figure QA has an OK font row", any(qa$check == "Font" & qa$status == "OK"))
})

# ===========================================================================
# 5. Paper style
# ===========================================================================
cat("\n-- 5. the paper style is saved, auto-loaded and forgotten --\n")
style_path <- file.path(work, "styles", "paper_style.json")
Sys.setenv(CFU_PAPER_STYLE = style_path)
style_font <- if (isTRUE(resolve_font_family("Times New Roman")$available)) "Times New Roman" else "serif"
shiny::testServer(app, {
  ok("with no file the status says so", grepl("^No paper style saved", output$paper_style_status))
  load_csv_in(session, raw_data, csv_abc)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])
  do.call(session$setInputs, c(
    list(font_family = style_font, font_size = 14, download_width = 5.5, plot_title = "Figure 2 only", y_min = 3),
    setNames(list(chosen_b), sample_colour_input_id("B"))
  ))
  session$setInputs(paper_style_save = 1)
  ok("Save writes the file where CFU_PAPER_STYLE points", file.exists(style_path))
  ok("the status names the file and when it was saved",
     startsWith(output$paper_style_status, paste0("Paper style: ", style_path, ", saved ")), output$paper_style_status)
})
st <- if (file.exists(style_path)) jsonlite::read_json(style_path, simplifyVector = TRUE) else list()
ok("the file is a paper style", identical(st$kind, "paper_style"))
ok("it records the font", identical(st$settings$font_family, style_font))
ok("it records the sample palette by name", identical(st$settings$sample_palette$B, chosen_b))
ok("it records the time palette", length(st$settings$time_palette) >= 2)
ok("it is loadable as a preset (same shape)", !is.null(st$settings) && identical(st$app, "CFU Plot Studio"))

shiny::testServer(app, {
  # A fresh session: the style is applied after the first flush.
  load_csv_in(session, raw_data, csv_be)
  applied <- last_applied_settings()
  # (The mock session never moves an input, so what the style SENT is read
  # from last_applied_settings(); the registry it merged is read directly.)
  do.call(session$setInputs, base_inputs)
  ok("a new session auto-loads the style", !is.null(applied))
  ok("font applied", identical(applied$font_family, style_font), format(applied$font_family))
  ok("sizes applied", identical(as.numeric(applied$font_size), 14) && identical(as.numeric(applied$download_width), 5.5))
  ok("plot_title NOT applied", is.null(applied$plot_title))
  ok("y_min NOT applied", is.null(applied$y_min))
  ok("no figure-specific setting applied", !any(unname(figure_specific_setting_ids) %in% names(applied)),
     paste(intersect(unname(figure_specific_setting_ids), names(applied)), collapse = ","))
  ok("palettes applied: B has the paper's colour", identical(colour_registry()$samples[["B"]], chosen_b))
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])
  m <- key_mapping()$entries
  ok("and the figure draws it", identical(m$fill[m$name == "B"], chosen_b))
  ok("the status names the style", startsWith(output$paper_style_status, paste0("Paper style: ", style_path, ", saved ")))
  # Uploading the same file as a preset keeps today's behaviour: everything.
  session$setInputs(preset_file = list(datapath = style_path, name = "paper_style.json"))
  ok("loaded as a preset it applies the figure settings too",
     identical(last_applied_settings()$plot_title, "Figure 2 only"))
  session$setInputs(paper_style_forget = 1)
  ok("Forget asks first (the file is still there)", file.exists(style_path))
  session$setInputs(paper_style_forget_confirm = 1)
  ok("Forget deletes the file once confirmed", !file.exists(style_path))
  ok("and resets the status", grepl("^No paper style saved", output$paper_style_status))
})
shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_be)
  ok("after Forget a new session loads nothing", is.null(last_applied_settings()))
})
Sys.setenv(CFU_PAPER_STYLE = no_style)

# ===========================================================================
# 6. recreate_figure.R
# ===========================================================================
cat("\n-- 6. the bundled recreate_figure.R keeps the palette and the font --\n")
zipfile <- file.path(work, "bundle.zip")
app_fills <- NULL; app_fonts <- NULL; reg_used <- NULL
rec_font <- if (isTRUE(resolve_font_family("Times New Roman")$available)) "Times New Roman" else "serif"
shiny::testServer(app, {
  load_csv_in(session, raw_data, csv_abc)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1], font_family = rec_font,
                    chart_geom = "bar")
  do.call(session$setInputs, setNames(list(chosen_b, "#117733"),
                                       c(sample_colour_input_id("B"), sample_colour_input_id("C"))))
  reg_used <<- colour_registry()$samples[c("A", "B", "C")]
  fv <- svg_parts(output$download_svg)
  app_fills <<- sort(unique(fv$glyph_fill[!is.na(fv$glyph_fill) & fv$glyph_fill != "NONE"]))
  app_fonts <<- unique(fv$text_font)
  file.copy(output$download_all, zipfile, overwrite = TRUE)
})
ex <- file.path(work, "bundle")
dir.create(ex, showWarnings = FALSE)
zip::unzip(zipfile, exdir = ex)
script <- readLines(file.path(ex, "recreate_figure.R"), warn = FALSE)
ok("the script embeds the sample palette", any(grepl("sample_palette", script, fixed = TRUE)) &&
     any(grepl(chosen_b, script, fixed = TRUE)))
ok("the script embeds the font", any(grepl(rec_font, script, fixed = TRUE)))
old <- setwd(ex)
rs <- suppressWarnings(system2(rscript, c("-e", shQuote(paste(
  "source('recreate_figure.R');",
  "svglite::svglite('recreated.svg', width = 7, height = 4.4); print(p); invisible(dev.off())"
))), stdout = TRUE, stderr = TRUE))
setwd(old)
ok("the script runs in a fresh R", identical(as.integer(attr(rs, "status") %||% 0L), 0L), paste(tail(rs, 5), collapse = "\n"))
if (file.exists(file.path(ex, "recreated.svg"))) {
  rv <- svg_parts(file.path(ex, "recreated.svg"))
  rec_fills <- sort(unique(rv$glyph_fill[!is.na(rv$glyph_fill) & rv$glyph_fill != "NONE"]))
  ok("the recreated figure draws every registry colour", all(norm_hex(reg_used) %in% rec_fills),
     paste(rec_fills, collapse = ","))
  ok("the recreated figure draws the same fills as the app", identical(rec_fills, app_fills),
     paste(paste(rec_fills, collapse = ","), "vs", paste(app_fills, collapse = ",")))
  ok(sprintf("the recreated figure is drawn in %s", rec_font), length(rv$text_font) > 0 &&
       all(not_symbol(rv) == rec_font) && identical(unique(rv$text_font), app_fonts),
     paste(unique(rv$text_font), collapse = ","))
} else {
  ok("the script's figure was drawn", FALSE)
}

unlink(work, recursive = TRUE)
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL PAPER STYLE TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
