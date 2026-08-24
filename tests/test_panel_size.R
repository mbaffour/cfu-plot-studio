# Fixed panel geometry tests. Run from the project root:
#   Rscript tests/test_panel_size.R
#
# The claim under test: with size_mode = "panel" the DATA AREA is identical
# across figures no matter how much room the legend, titles and caption take.
# Measured off the rendered gtable, never asserted from the settings.
# Exits non-zero on failure.

suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(grid)})
app_lines <- readLines("app.R", warn = FALSE)
eval(parse(text = paste(app_lines[1:(grep("^ui <- fluidPage", app_lines)[1] - 1)], collapse = "\n")),
     envir = globalenv())

fails <- 0
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) cat(sprintf("  PASS  %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("  FAIL  %s   %s\n", label, detail)) }
}
eq <- function(a, b, tol = 1e-6) isTRUE(all.equal(unname(a), unname(b), tolerance = tol))
pw_of <- function(x) unname(x["w"])
ph_of <- function(x) unname(x["h"])

# Measure the real panel rectangle of a built object, in inches.
measured_panel <- function(obj, fig_w, fig_h) {
  g <- if (inherits(obj, "gtable")) obj else ggplotGrob(obj)
  f <- tempfile(fileext = ".png")
  png(f, width = fig_w, height = fig_h, units = "in", res = 150)
  on.exit({dev.off(); unlink(f)}, add = TRUE)
  grid.newpage(); grid.draw(g)
  lay <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
  w <- convertWidth(g$widths[unique(lay$l)][1], "in", valueOnly = TRUE)
  h <- convertHeight(g$heights[unique(lay$t)][1], "in", valueOnly = TRUE)
  if (w == 0) {
    # a null unit reports zero: the panel is taking whatever is left over
    w <- fig_w - convertWidth(sum(g$widths[-unique(lay$l)]), "in", valueOnly = TRUE)
    h <- fig_h - convertHeight(sum(g$heights[-unique(lay$t)]), "in", valueOnly = TRUE)
  }
  c(w = w, h = h)
}

base_inp <- list(
  plot_mode = "combined", comparison = "none", stats_method = "welch", p_adjust = "BH",
  p_adjust_scope = "global", label_kind = "stars", show_ns = FALSE,
  y_mode = "log10", chart_geom = "bar", bar_color_mode = "group",
  error_type = "SD", variation_display = "errorbar", show_points = TRUE,
  y_min = 0, y_max = NA, show_method_caption = TRUE, show_subtitle = FALSE,
  hide_subtitle_no_stats = TRUE, show_n_labels = FALSE,
  plot_title = "T", plot_subtitle = "", x_label = "Treatment", y_label = "CFU/mL",
  legend_title = "S", treatment_unit = "", append_treatment_unit = FALSE,
  time_unit = "min", append_time_unit = TRUE, bar_orientation = "vertical",
  plot_theme = "classic", plot_box = FALSE, show_y_ticks = TRUE,
  show_minor_y_ticks = FALSE, show_y_grid = FALSE, show_minor_y_grid = FALSE,
  y_major_step = NA, y_minor_step = NA, y_tick_length = 4, minor_y_tick_length = 2,
  axis_line_width = .6, box_line_width = .6, axis_color = "#262626",
  grid_color = "#DDD", bar_outline_color = "#262626", bar_outline_width = .25,
  errorbar_width = .55, sample_color_1 = "#0072B2", sample_color_2 = "#D55E00",
  time_color_1 = "#56B4E9", time_color_2 = "#0072B2", single_color = "#0072B2",
  stat_color = "#262626", bar_width = .68, dodge_width = .78, point_size = 1.8,
  point_alpha = .9, jitter_width = .08, jitter_seed = 1, font_size = 11,
  title_size = 12, subtitle_size = 9, stat_size = 3, x_angle = 0,
  legend_position = "top", legend_x = .98, legend_y = .98,
  legend_just_x = 1, legend_just_y = 1, title_hjust = 0, subtitle_hjust = 0,
  caption_hjust = 0, x_title_hjust = .5, y_title_hjust = .5,
  size_units = "in", size_mode = "total",
  download_width = 7, download_height = 4.4, download_dpi = 300
)

raw <- tibble::tribble(~Sample, ~inducer_concentration, ~Time, ~Replicate, ~CFU,
  "Short", 0, 0, 1, 1e8, "Short", 0, 0, 2, 1.2e8, "Short", 0, 0, 3, 0.9e8,
  "Short", 0,120, 1, 1e7, "Short", 0,120, 2, 1.1e7, "Short", 0,120, 3, 0.8e7,
  "Other", 0, 0, 1, 1e8, "Other", 0, 0, 2, 1.1e8, "Other", 0, 0, 3, 1.0e8,
  "Other", 0,120, 1, 5e7, "Other", 0,120, 2, 6e7, "Other", 0,120, 3, 4e7)
d <- prep_cfu_data(raw, list(sample = "Sample", concentration = "inducer_concentration",
                             time = "Time", replicate = "Replicate", cfu = "CFU"),
                   "", "min", FALSE, TRUE)

build <- function(overrides = list()) {
  i <- modifyList(base_inp, overrides)
  sm <- plot_summary(d, i$plot_mode, i$y_mode, i$error_type)
  make_cfu_plot(d, sm, tibble::tibble(), i$plot_mode, i$y_mode, i$error_type, i)
}

plain <- build()
loud  <- build(list(plot_title = paste(rep("A very long title", 4), collapse = " "),
                    legend_title = "An extremely long legend title indeed",
                    comparison = "sample",
                    x_label = paste(rep("long x axis label", 3), collapse = " ")))
tall  <- build(list(x_angle = 60, show_n_labels = TRUE, legend_position = "bottom",
                    plot_title = "Two\nline\ntitle"))

# The classic complaint: a right-hand legend eats panel WIDTH.
wide <- build(list(legend_position = "right",
                   legend_title = "Construct and induction condition"))

cat("\n-- the problem this exists to solve --\n")
ps <- lapply(list(plain = plain, loud = loud, tall = tall, wide = wide),
             measured_panel, fig_w = 7, fig_h = 4.4)
for (nm in names(ps)) cat(sprintf("     %-6s panel = %.3f x %.3f in\n", nm, pw_of(ps[[nm]]), ph_of(ps[[nm]])))
ws <- vapply(ps, pw_of, numeric(1)); hs <- vapply(ps, ph_of, numeric(1))
ok("unpinned panel WIDTH changes when the legend moves to the side",
   (max(ws) - min(ws)) > 0.2, sprintf("width spread %.3f in", max(ws) - min(ws)))
ok("unpinned panel HEIGHT changes with the title and axis labels",
   (max(hs) - min(hs)) > 0.2, sprintf("height spread %.3f in", max(hs) - min(hs)))

cat("\n-- pinned: the data area is identical --\n")
PW <- 4.0; PH <- 2.4
pinned <- lapply(list(plain = plain, loud = loud, tall = tall, wide = wide), fix_panel_size, PW, PH)
for (nm in names(pinned)) {
  m <- measured_panel(pinned[[nm]], 14, 12)
  ok(sprintf("%-6s panel is exactly %.2f x %.2f in", nm, PW, PH),
     eq(pw_of(m), PW, tol = 1e-4) && eq(ph_of(m), PH, tol = 1e-4),
     sprintf("got %.4f x %.4f", pw_of(m), ph_of(m)))
}

cat("\n-- the figure grows around the panel instead --\n")
geos <- lapply(pinned, gtable_size_in, dpi = 150)
for (nm in names(geos)) cat(sprintf("     %-6s figure = %.3f x %.3f in\n", nm, geos[[nm]]$width, geos[[nm]]$height))
ok("every computed figure is larger than the panel it contains",
   all(vapply(geos, function(g) g$width > PW && g$height > PH, logical(1))))
ok("the loud figure needs more room than the plain one",
   geos$loud$width > geos$plain$width || geos$loud$height > geos$plain$height,
   sprintf("plain %.3f x %.3f, loud %.3f x %.3f",
           geos$plain$width, geos$plain$height, geos$loud$width, geos$loud$height))
ok("no layout cell is left unresolved",
   all(vapply(geos, function(g) g$unresolved == 0, logical(1))),
   paste(vapply(geos, function(g) g$unresolved, numeric(1)), collapse = ","))

cat("\n-- facets: every panel gets the size, so panels stay comparable --\n")
gf <- fix_panel_size(build(list(plot_mode = "combined")), PW, PH)
layf <- gf$layout[grepl("^panel", gf$layout$name), , drop = FALSE]
ok("the faceted figure really has more than one panel", nrow(layf) >= 2, paste("panels:", nrow(layf)))
mf <- measured_panel(gf, 20, 12)
ok("each facet panel is the requested size", eq(pw_of(mf), PW, tol = 1e-4) && eq(ph_of(mf), PH, tol = 1e-4),
   sprintf("got %.4f x %.4f", pw_of(mf), ph_of(mf)))
gsz <- gtable_size_in(gf, dpi = 150)
ok("total width covers every panel", gsz$width > nrow(layf) * PW * 0.9,
   sprintf("total %.3f for %d panels of %.2f", gsz$width, nrow(layf), PW))

cat("\n-- survival mode pins the same way --\n")
sv <- pair_survival(d, "0 min", "120 min")
si <- modifyList(base_inp, list(plot_mode = "survival", surv_baseline = "0 min",
                                surv_readout = "120 min", surv_scale = "log10",
                                y_min = NA, y_max = NA))
ssm <- plot_summary(sv, "survival", "log10", "SD", clamp_zero = FALSE)
sp <- make_cfu_plot(sv, ssm, tibble::tibble(), "survival", "log10", "SD", si)
ms <- measured_panel(fix_panel_size(sp, PW, PH), 14, 12)
ok("survival panel is pinned too", eq(pw_of(ms), PW, tol = 1e-4) && eq(ph_of(ms), PH, tol = 1e-4),
   sprintf("got %.4f x %.4f", pw_of(ms), ph_of(ms)))

cat("\n-- guards --\n")
ok("a non-finite panel size leaves the plot alone",
   inherits(fix_panel_size(plain, NA_real_, NA_real_), "gtable"))
ok("pinning is idempotent",
   eq(pw_of(measured_panel(fix_panel_size(fix_panel_size(plain, PW, PH), PW, PH), 14, 12)), PW, tol = 1e-4))
ok("a gtable can be pinned directly, not only a ggplot",
   inherits(fix_panel_size(ggplotGrob(plain), PW, PH), "gtable"))
f <- tempfile(fileext = ".png")
r <- tryCatch({ ggsave(f, pinned$loud, width = geos$loud$width, height = geos$loud$height,
                       units = "in", dpi = 100); "ok" }, error = function(e) conditionMessage(e))
ok("a pinned gtable saves through ggsave", identical(r, "ok") && file.size(f) > 5000, r)
unlink(f)

cat(sprintf("\n%s\n", strrep("-", 60)))
if (fails == 0) cat("ALL PANEL SIZE TESTS PASSED\n") else {
  cat(sprintf("%d TEST(S) FAILED\n", fails)); quit(status = 1)
}
