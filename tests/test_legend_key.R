# Legend key test. Run from the project root:
#   Rscript tests/test_legend_key.R
#
# Drives the real server on a three-sample fixture and checks, from the files
# the download handlers write, that the legend key and the legend text starter
# match the figure's own colour mapping, follow renames / recolours / selection,
# match the key placed on the figure, and keep their names as live text.
# Exits non-zero on failure.

suppressPackageStartupMessages({library(shiny); library(ggplot2); library(dplyr); library(tidyr)})
# Never pick up a paper style saved beside app.R: it would restyle the fixture.
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style.json"))

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}

app_lines <- readLines("app.R", warn = FALSE)
eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\n")),
     envir = globalenv())
app <- shiny::as.shiny.appobj(source("app.R", local = new.env())$value)

# --- fixture: three samples, two treatments, two timepoints, three replicates --
fixture <- function(names) {
  grid <- expand.grid(Replicate = 1:3, Timepoint = c("Early", "Late"),
                      Treatment = c("Baseline", "Treatment A"), Group = names,
                      stringsAsFactors = FALSE)
  set.seed(7)
  grid$CFU <- round(10^(6 + as.integer(factor(grid$Group, levels = names)) * 0.3 +
                          (grid$Timepoint == "Late") * 0.5 + rnorm(nrow(grid), 0, 0.1)))
  f <- tempfile(fileext = ".csv")
  write.csv(grid[, c("Group", "Treatment", "Timepoint", "Replicate", "CFU")], f, row.names = FALSE)
  f
}
names1 <- c("MG1655", "MG1655 + WT", "MG1655 + mutant")
names2 <- c("MG1655", "MG1655 + WT (complemented)", "MG1655 + mutant")   # one renamed
csv1 <- fixture(names1)
csv2 <- fixture(names2)

# --- reading the written files ----------------------------------------------
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
  num <- function(x) suppressWarnings(as.numeric(x))   # "100%" on the page rect
  list(
    text = xml2::xml_text(texts),
    text_y = as.numeric(xml2::xml_attr(texts, "y")),
    text_x = as.numeric(xml2::xml_attr(texts, "x")),
    text_font = style_val(texts, "font-family"),
    glyph_name = xml2::xml_name(glyphs),
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
    }, numeric(1)),
    n_path = length(xml2::xml_find_all(doc, ".//path"))
  )
}

# The glyph drawn on each key row: the coloured glyph just left of the label and
# nearest its line (so data points elsewhere on a figure are never picked).
# Background rects are white with no stroke, so they are skipped.
row_fill <- function(sv, label) {
  i <- match(label, sv$text)
  if (is.na(i)) return(NA_character_)
  coloured <- which(!is.na(sv$glyph_stroke) & sv$glyph_stroke != "NONE" &
                      sv$glyph_x < sv$text_x[i] & sv$glyph_x > sv$text_x[i] - 30)
  if (length(coloured) == 0) return(NA_character_)
  j <- coloured[which.min(abs(sv$glyph_y[coloured] - sv$text_y[i]))]
  sv$glyph_fill[j]
}

# Walk a grob tree in drawing order and collect every label and fill: a
# fingerprint of the key's entries, independent of where it is drawn.
grob_fingerprint <- function(g) {
  labels <- character(0); fills <- character(0)
  walk <- function(x) {
    if (inherits(x, "text") || inherits(x, "titleGrob")) labels <<- c(labels, as.character(x$label))
    if (!is.null(x$gp$fill) && !inherits(x, "text")) fills <<- c(fills, as.character(x$gp$fill))
    kids <- c(x$grobs, x$children)
    for (k in kids) walk(k)
  }
  walk(g)
  list(labels = labels, fills = fills)
}

pdf_text_ops <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  # Inflate every content stream and count text-showing operators. Names drawn
  # as outlines would leave no Tj/TJ behind.
  starts <- grepRaw("stream", bytes, all = TRUE, fixed = TRUE)
  ends <- grepRaw("endstream", bytes, all = TRUE, fixed = TRUE)
  starts <- starts[!vapply(starts, function(s) s > 3 &&
                             identical(rawToChar(bytes[(s - 3):(s - 1)]), "end"), logical(1))]
  n <- 0L
  for (s in starts) {
    from <- s + 6L
    while (from <= length(bytes) && bytes[from] %in% as.raw(c(13, 10))) from <- from + 1L
    e <- ends[ends > from][1]
    if (is.na(e)) next
    chunk <- bytes[from:(e - 1L)]
    inflated <- tryCatch(memDecompress(chunk, type = "gzip"), error = function(err) chunk)
    n <- n + length(grepRaw("Tj", inflated, all = TRUE, fixed = TRUE)) +
      length(grepRaw("TJ", inflated, all = TRUE, fixed = TRUE))
  }
  list(has_font = length(grepRaw("/BaseFont", bytes, fixed = TRUE)) > 0, text_ops = n)
}

base_inputs <- list(
  stats_method = "welch", p_adjust = "BH", p_adjust_scope = "global", label_kind = "stars",
  show_ns = FALSE, show_points = TRUE, variation_display = "errorbar", bar_color_mode = "group",
  chart_geom = "point", y_mode = "log10", error_type = "SD", y_min = NA, y_max = NA,
  show_method_caption = TRUE, show_subtitle = FALSE, show_n_labels = TRUE,
  plot_theme = "classic", bar_orientation = "vertical", legend_position = "right",
  plot_title = "Legend key check", plot_subtitle = "", x_label = "Treatment",
  y_label = "CFU/mL", legend_title = "", treatment_unit = "", time_unit = "min",
  append_treatment_unit = FALSE, append_time_unit = TRUE,
  size_mode = "total", size_units = "in", download_width = 7, download_height = 4.4,
  download_dpi = 150, jitter_seed = 1, canvas_mode = "off", comparison = "none",
  ppt_editable = TRUE, bundle_gif = FALSE, font_size = 11, single_color = "#7AA6C2",
  bar_outline_color = "#262626", bar_outline_width = 0.25, legend_key_on_figure = FALSE
)
# Colours are picked per sample NAME (0.4.0): black and orange for the first two.
colour_inputs <- setNames(list("#000000", "#FF7F00", "#7AA6C2", "#2F5D8C"),
                          c(sample_colour_input_id(names1[1]), sample_colour_input_id(names1[2]),
                            time_colour_input_id("Early"), time_colour_input_id("Late")))
base_inputs <- c(base_inputs, colour_inputs)

zipfile <- tempfile(fileext = ".zip")
starter_in_app <- NULL

shiny::testServer(app, {
  load_csv <- function(path) {
    session$setInputs(file = list(datapath = normalizePath(path), name = basename(path)))
    cols <- names(raw_data())
    session$setInputs(col_sample = cols[1], col_conc = cols[2], col_time = cols[3],
                      col_rep = cols[4], col_cfu = cols[5])
  }
  load_csv(csv1)
  do.call(session$setInputs, base_inputs)
  d <- cfu_data()
  session$setInputs(plot_mode = "combined", samples = levels(d$sample), times = levels(d$time_min),
                    control_concentration = levels(d$concentration_label)[1])

  # ---- 1. every group in the figure's mapping is in the key, exactly -------
  cat("\n-- 1. the key is the figure's mapping --\n")
  map <- key_mapping()
  ok("the mapping has the three fixture samples", setequal(map$entries$name, names1),
     paste(map$entries$name, collapse = " | "))
  ok("two different colours are in play", length(unique(map$entries$fill)) >= 2)
  # The mapping is what the figure actually drew: compare with ggplot's built data.
  built <- ggplot2::ggplot_build(current_plot())
  drawn_fills <- unique(norm_hex(unlist(lapply(built$data, function(l) l$fill[!is.na(l$fill)]))))
  ok("the figure draws exactly the mapping's fills",
     setequal(drawn_fills, norm_hex(map$entries$fill)),
     paste("drawn:", paste(drawn_fills, collapse = ","), "map:", paste(map$entries$fill, collapse = ",")))

  key_svg <- output$download_key_svg
  sv <- svg_parts(key_svg)
  for (k in seq_len(nrow(map$entries))) {
    nm <- map$entries$name[k]
    ok(sprintf("key names '%s'", nm), nm %in% sv$text)
    ok(sprintf("key draws '%s' in %s", nm, map$entries$fill[k]),
       identical(row_fill(sv, nm), norm_hex(map$entries$fill[k])), row_fill(sv, nm))
  }
  ok("key glyphs are circles, as on the figure (shape 21)",
     map$entries$glyph[1] == "point" && sum(sv$glyph_name == "circle") >= nrow(map$entries))
  ok("key outlines are the figure's outline colour",
     all(sv$glyph_stroke[sv$glyph_name == "circle"] == norm_hex(base_inputs$bar_outline_color)))
  fig_svg <- output$download_svg
  fv <- svg_parts(fig_svg)
  ok("key text is in the figure's font",
     length(unique(sv$text_font)) == 1 && unique(sv$text_font) %in% fv$text_font,
     paste(unique(sv$text_font), "vs", paste(unique(fv$text_font), collapse = ",")))

  # Bars too: a second glyph type, drawn as rectangles in the figure's fill.
  session$setInputs(chart_geom = "bar")
  bv <- svg_parts(output$download_key_svg)
  ok("bar figures key with filled rectangles of the bar colour",
     identical(row_fill(bv, names1[2]), norm_hex(colour_inputs[[2]])), row_fill(bv, names1[2]))
  ok("bar text starter says bars", grepl("MG1655 + WT (orange, bars", legend_text(), fixed = TRUE),
     legend_text())
  session$setInputs(chart_geom = "point")

  # ---- 2. rename + recolour + selection are followed, never stale -----------
  cat("\n-- 2. renames, recolours and selection are followed --\n")
  load_csv(csv2)
  d2 <- cfu_data()
  # The renamed sample is a new name, so it is given orange the way its old
  # name was; then MG1655 is recoloured black -> blue.
  do.call(session$setInputs, setNames(list(levels(d2$sample), levels(d2$time_min), "#FF7F00", "#377EB8"),
                                      c("samples", "times", sample_colour_input_id(names2[2]),
                                        sample_colour_input_id("MG1655"))))
  sv2 <- svg_parts(output$download_key_svg)
  ok("the renamed sample is in the key", "MG1655 + WT (complemented)" %in% sv2$text)
  ok("the old name is gone from the key", !("MG1655 + WT" %in% sv2$text))
  ok("the recoloured sample is drawn in its new colour",
     identical(row_fill(sv2, "MG1655"), "#377EB8"), row_fill(sv2, "MG1655"))
  txt2 <- legend_text()
  ok("the text starter has the new name", grepl("MG1655 + WT (complemented) (", txt2, fixed = TRUE), txt2)
  ok("the text starter has the new colour word", grepl("MG1655 (blue,", txt2, fixed = TRUE), txt2)
  ok("the text starter dropped the old name", !grepl("MG1655 + WT (orange", txt2, fixed = TRUE), txt2)
  session$setInputs(samples = c("MG1655", "MG1655 + mutant"))
  sv3 <- svg_parts(output$download_key_svg)
  ok("a deselected sample leaves the key", !("MG1655 + WT (complemented)" %in% sv3$text) &&
       all(c("MG1655", "MG1655 + mutant") %in% sv3$text))
  ok("and leaves the text starter", !grepl("complemented", legend_text(), fixed = TRUE))
  session$setInputs(samples = levels(d2$sample))

  # ---- 3. the text starter ---------------------------------------------------
  cat("\n-- 3. the legend text starter --\n")
  txt <- legend_text()
  starter_in_app <<- txt
  cat("     ", txt, "\n")
  m3 <- key_mapping()
  pieces <- strsplit(sub("\\.( .*)?$", "", txt), "; ", fixed = TRUE)[[1]]
  ok("one entry per group", length(pieces) == nrow(m3$entries), paste(length(pieces), "vs", nrow(m3$entries)))
  for (k in seq_len(nrow(m3$entries))) {
    nm <- m3$entries$name[k]
    piece <- pieces[startsWith(pieces, paste0(nm, " ("))]
    word <- if (length(piece) == 1) sub(",.*$", "", sub(paste0("^", gsub("([][()+.])", "\\\\\\1", nm), " \\("), "", piece)) else NA
    base_word <- sub("^(light|dark|lighter|darker) (?=.)", "", word, perl = TRUE)
    ok(sprintf("'%s' has a colour word (%s)", nm, word),
       !is.na(word) && (word %in% names(legend_colour_table) || base_word %in% names(legend_colour_table)))
  }
  entry_names <- sub(" \\([^()]*\\)$", "", pieces)
  ok("entry names are unique", !anyDuplicated(entry_names))
  ok("near-identical colours are told apart",
     length(unique(colour_words(c("#377EB8", "#2A6496")))) == 2,
     paste(colour_words(c("#377EB8", "#2A6496")), collapse = " / "))
  ok("a figure's text starter names its marker", grepl("filled circles", txt, fixed = TRUE))

  # ---- 4. the key placed on the figure is the standalone key -----------------
  cat("\n-- 4. embedded key == standalone key --\n")
  session$setInputs(legend_position = "none")
  standalone_fp <- grob_fingerprint(legend_key()$grob)
  standalone_sv <- svg_parts(output$download_key_svg)
  off_fig <- svg_parts(output$download_svg)
  session$setInputs(legend_key_on_figure = TRUE)
  on_fig <- svg_parts(output$download_svg)
  embedded <- legend_key_grob(current_plot())
  embedded_fp <- grob_fingerprint(embedded$grob)
  ok("the switch puts a key on a figure that had no legend", !is.null(embedded))
  ok("embedded and standalone keys list the same labels, in order",
     identical(embedded_fp$labels, standalone_fp$labels),
     paste(paste(embedded_fp$labels, collapse = "|"), "vs", paste(standalone_fp$labels, collapse = "|")))
  ok("embedded and standalone keys draw the same fills, in order",
     identical(embedded_fp$fills, standalone_fp$fills))
  added <- setdiff(on_fig$text, off_fig$text)
  ok("the figure file gained exactly the key's names",
     setequal(added, standalone_sv$text), paste(added, collapse = " | "))
  for (nm in standalone_sv$text) {
    ok(sprintf("embedded '%s' is drawn in the key's colour", nm),
       identical(row_fill(on_fig, nm), row_fill(standalone_sv, nm)),
       paste(row_fill(on_fig, nm), "vs", row_fill(standalone_sv, nm)))
  }
  session$setInputs(legend_position = "right", legend_key_on_figure = FALSE)

  # ---- 5. vector formats keep names as text ---------------------------------
  cat("\n-- 5. names stay live text in vector formats --\n")
  sv5 <- svg_parts(output$download_key_svg)
  ok("SVG key: every name is a <text> element",
     all(key_mapping()$entries$name %in% sv5$text))
  ok("SVG key: no glyph outlines stand in for text", sv5$n_path == 0, paste(sv5$n_path, "paths"))
  pdf_path <- output$download_key_pdf
  ok("PDF key is a real PDF", file_looks_like(pdf_path, "pdf"))
  pt <- pdf_text_ops(pdf_path)
  ok("PDF key uses fonts", pt$has_font)
  ok("PDF key draws its names with text operators", pt$text_ops >= nrow(key_mapping()$entries),
     paste(pt$text_ops, "text operators"))
  ok("PNG key is a real PNG", file_looks_like(output$download_key_png, "png"))

  # ---- every figure the app makes -------------------------------------------
  cat("\n-- every plot mode has a key and a text starter --\n")
  tl <- levels(d2$time_min)
  modes <- list(
    combined = list(plot_mode = "combined", bar_color_mode = "group"),
    survival = list(plot_mode = "survival", bar_color_mode = "group",
                    surv_baseline = tl[1], surv_readout = tl[length(tl)], surv_scale = "log10",
                    surv_match_replicates = FALSE),
    sample_both = list(plot_mode = "sample_both", bar_color_mode = "group",
                       single_sample = levels(d2$sample)[1], times = tl),
    sample_time = list(plot_mode = "sample_time", bar_color_mode = "group",
                       single_sample = levels(d2$sample)[2], single_time = tl[1]),
    per_bar = list(plot_mode = "combined", bar_color_mode = "manual", times = tl)
  )
  for (nm in names(modes)) {
    do.call(session$setInputs, c(modes[[nm]], list(samples = levels(d2$sample))))
    m <- key_mapping()
    k <- tryCatch(legend_key(), error = function(e) NULL)
    kv <- tryCatch(svg_parts(output$download_key_svg), error = function(e) NULL)
    ok(sprintf("%s: key built", nm), !is.null(k) && !is.null(kv))
    ok(sprintf("%s: key names every mapped group", nm),
       !is.null(kv) && setequal(intersect(kv$text, m$entries$name), m$entries$name),
       paste(setdiff(m$entries$name, kv$text), collapse = " | "))
    ok(sprintf("%s: text starter names every group", nm),
       all(vapply(m$entries$name, function(x) grepl(paste0(x, " ("), legend_text(), fixed = TRUE), logical(1))))
    if (nm == "sample_time") {
      ok("sample_time: the one entry is the sample shown", identical(m$entries$name, levels(d2$sample)[2]),
         paste(m$entries$name, collapse = " | "))
    }
    rv <- make_reveal_plot(plot_data(), current_summary(), input$plot_mode, input$y_mode,
                           input$error_type, input, step = NULL, bar_palette = bar_palette())
    ok(sprintf("%s: reveal slides follow the switch (off -> figure's usual legend)", nm),
       is.null(legend_key_grob(rv)) == (nm %in% c("sample_time", "per_bar")))
  }

  # ---- the bundle ------------------------------------------------------------
  cat("\n-- the bundle carries the key --\n")
  do.call(session$setInputs, modes$combined)
  # comparison = "auto" so the figure carries statistic labels and a methods
  # caption: the fresh-R run below then also exercises the caption helpers.
  session$setInputs(samples = levels(d2$sample), times = tl, comparison = "auto")
  starter_in_app <<- legend_text()
  produced <- output$download_all
  file.copy(produced, zipfile, overwrite = TRUE)
})

listing <- zip::zip_list(zipfile)
for (e in c("legend_key.png", "legend_key.pdf", "legend_key.svg", "legend_text.txt")) {
  ok(sprintf("bundle contains %s", e), e %in% listing$filename)
}
ex <- file.path(tempdir(), paste0("keybundle_", as.integer(runif(1, 1, 1e9))))
dir.create(ex, recursive = TRUE, showWarnings = FALSE)
zip::unzip(zipfile, exdir = ex)
ok("bundled key SVG is SVG", file_looks_like(file.path(ex, "legend_key.svg"), "svg"))
ok("bundled key PDF is PDF", file_looks_like(file.path(ex, "legend_key.pdf"), "pdf"))
bundled_txt <- paste(readLines(file.path(ex, "legend_text.txt"), warn = FALSE), collapse = "\n")
ok("bundled legend_text.txt is the app's text starter", identical(bundled_txt, starter_in_app),
   bundled_txt)
ok("the bundle README lists the key", any(grepl("legend_key.svg", readLines(file.path(ex, "README.txt")), fixed = TRUE)))
bk <- svg_parts(file.path(ex, "legend_key.svg"))
ok("bundled key names the current samples", all(names2 %in% bk$text))

# The recreate script must rebuild the key in a CLEAN R process: the app's
# helpers are not around to paper over a function the script forgot to carry.
cat("\n-- the recreate script rebuilds the key on its own --\n")
# system2 has no working-directory argument; run it from the bundle directory.
old <- setwd(ex)
rs <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                               c("-e", shQuote("source('recreate_figure.R')")),
                               stdout = TRUE, stderr = TRUE))
setwd(old)
status <- attr(rs, "status") %||% 0
ok("the bundled script runs in a fresh R", identical(as.integer(status), 0L), paste(tail(rs, 5), collapse = "\n"))
ok("it wrote the legend key", file.exists(file.path(ex, "legend_key_recreated.png")) &&
     file_looks_like(file.path(ex, "legend_key_recreated.png"), "png"))
rec_txt <- if (file.exists(file.path(ex, "legend_text_recreated.txt")))
  paste(readLines(file.path(ex, "legend_text_recreated.txt"), warn = FALSE), collapse = "\n") else ""
ok("and the same text starter as the app", identical(rec_txt, starter_in_app), rec_txt)

unlink(ex, recursive = TRUE); unlink(zipfile); unlink(c(csv1, csv2))
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL LEGEND KEY TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
