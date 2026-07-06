suppressPackageStartupMessages({
  suppressWarnings(library(shiny))
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(dplyr))
  suppressWarnings(library(readr))
  suppressWarnings(library(emmeans))
  suppressWarnings(library(broom))
  suppressWarnings(library(DT))
  suppressWarnings(library(colourpicker))
})

demo_file <- "dummy_cfu_example.csv"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

axis_limit <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) NA_real_ else as.numeric(x)
}

clean_names <- function(x) {
  trimws(x)
}

guess_column <- function(cols, candidates) {
  cols_clean <- tolower(gsub("[^a-z0-9]+", "", cols))
  candidates_clean <- tolower(gsub("[^a-z0-9]+", "", candidates))
  hit <- match(candidates_clean, cols_clean, nomatch = 0)
  if (any(hit > 0)) cols[hit[hit > 0][1]] else cols[1]
}

format_label <- function(x, unit = "", append_unit = TRUE) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (all(!is.na(x_num))) {
    ord <- order(x_num)
    unit_text <- trimws(unit %||% "")
    suffix <- if (isTRUE(append_unit) && nzchar(unit_text)) paste0(" ", unit_text) else ""
    labels <- paste0(x_num[ord], suffix)
    list(values = x_chr[ord], labels = labels, numeric = x_num[ord])
  } else {
    vals <- unique(x_chr)
    list(values = vals, labels = vals, numeric = seq_along(vals))
  }
}

axis_step_breaks <- function(min_val, max_val, step, log_base_10 = FALSE) {
  step <- suppressWarnings(as.numeric(step))
  if (length(step) == 0 || is.na(step) || !is.finite(step) || step <= 0) return(NULL)
  if (!is.finite(min_val) || !is.finite(max_val) || min_val >= max_val) return(NULL)

  if (isTRUE(log_base_10)) {
    min_val <- max(min_val, .Machine$double.eps)
    start <- floor(log10(min_val) / step) * step
    end <- ceiling(log10(max_val) / step) * step
    return(10^seq(start, end, by = step))
  }

  start <- ceiling(min_val / step) * step
  end <- floor(max_val / step) * step
  seq(start, end, by = step)
}

scale_breaks_or_default <- function(x) {
  if (is.null(x) || length(x) == 0) waiver() else x
}

plot_setting_ids <- c(
  "plot_mode", "comparison", "stats_method", "p_adjust", "p_adjust_scope", "label_kind", "show_ns",
  "y_mode", "error_type", "variation_display", "show_points", "y_min", "y_max",
  "plot_title", "plot_subtitle", "show_subtitle", "hide_subtitle_no_stats", "show_method_caption",
  "x_label", "treatment_unit", "append_treatment_unit", "time_unit", "append_time_unit",
  "legend_title", "bar_orientation", "plot_theme", "plot_box", "show_y_ticks",
  "show_minor_y_ticks", "show_y_grid", "show_minor_y_grid", "y_major_step", "y_minor_step",
  "y_tick_length", "minor_y_tick_length", "axis_line_width", "box_line_width",
  "axis_color", "grid_color", "bar_outline_color", "bar_outline_width", "errorbar_width",
  "sample_color_1", "sample_color_2", "time_color_1", "time_color_2", "single_color", "stat_color",
  "bar_width", "dodge_width", "point_size", "point_alpha", "jitter_width",
  "font_size", "title_size", "subtitle_size", "stat_size", "x_angle",
  "legend_position", "legend_x", "legend_y", "legend_just_x", "legend_just_y",
  "download_width", "download_height", "download_dpi", "animation_fps", "animation_duration",
  "animation_dpi", "ppt_editable"
)

select_setting_ids <- c(
  "plot_mode", "comparison", "stats_method", "p_adjust", "p_adjust_scope", "y_mode",
  "error_type", "variation_display", "bar_orientation", "plot_theme", "legend_position"
)

radio_setting_ids <- c("label_kind")

checkbox_setting_ids <- c(
  "show_ns", "show_points", "show_subtitle", "hide_subtitle_no_stats", "show_method_caption", "append_treatment_unit",
  "append_time_unit", "plot_box", "show_y_ticks", "show_minor_y_ticks", "show_y_grid",
  "show_minor_y_grid", "ppt_editable"
)

text_setting_ids <- c("plot_title", "plot_subtitle", "x_label", "treatment_unit", "time_unit", "legend_title")

numeric_setting_ids <- c(
  "y_min", "y_max", "y_major_step", "y_minor_step", "download_width", "download_height",
  "download_dpi", "animation_dpi"
)

slider_setting_ids <- setdiff(
  plot_setting_ids,
  c(select_setting_ids, radio_setting_ids, checkbox_setting_ids, text_setting_ids, numeric_setting_ids)
)

collect_plot_settings <- function(input) {
  out <- lapply(plot_setting_ids, function(id) input[[id]])
  names(out) <- plot_setting_ids
  Filter(function(x) !is.null(x) && length(x) > 0, out)
}

plot_settings_payload <- function(input) {
  list(
    app = "CFU Plot Studio",
    preset_version = 1,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    settings = collect_plot_settings(input)
  )
}

significance_label <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

# Okabe-Ito colorblind-safe qualitative palette (Wong 2011, Nat Methods 8:441).
# Order chosen so the first two entries read clearly for the common 2-group CFU case.
okabe_ito <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7",
  "#E69F00", "#56B4E9", "#F0E442", "#000000"
)

# Human-readable label for the variation/error-bar summary, for figure captions.
error_type_caption <- function(error_type, variation_display = "errorbar") {
  if (identical(variation_display, "none")) return(NULL)
  descr <- switch(
    error_type %||% "SD",
    "SD" = "error bars show mean ± SD",
    "SEM" = "error bars show mean ± SEM",
    "95% CI" = "error bars show mean with 95% CI",
    "IQR" = "intervals show median IQR (Q1-Q3)",
    "Range (min-max)" = "intervals show min-max range",
    paste0("error bars: ", error_type)
  )
  descr
}

# Names the statistical test + multiple-comparison correction for figure captions.
stats_caption <- function(stats_method, p_adjust) {
  test_txt <- switch(
    stats_method %||% "welch",
    "welch" = "Welch t-test on log10(CFU)",
    "student" = "Student t-test on log10(CFU)",
    "emmeans" = "linear model + emmeans on log10(CFU)",
    "statistical test"
  )
  corr_txt <- switch(
    p_adjust %||% "BH",
    "BH" = "Benjamini-Hochberg FDR",
    "holm" = "Holm",
    "bonferroni" = "Bonferroni",
    "none" = "no",
    p_adjust
  )
  paste0(test_txt, "; ", corr_txt, " correction")
}

named_palette <- function(levels_vec, seed_colors) {
  levels_vec <- as.character(levels_vec)
  seed_colors <- unname(seed_colors)
  if (length(levels_vec) <= length(seed_colors)) {
    setNames(seed_colors[seq_along(levels_vec)], levels_vec)
  } else {
    extra <- grDevices::hcl.colors(length(levels_vec) - length(seed_colors), palette = "Dark 3")
    setNames(c(seed_colors, extra), levels_vec)
  }
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "p<0.001", paste0("p=", signif(p, 2))))
}

format_q <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "q<0.001", paste0("q=", signif(p, 2))))
}

prep_cfu_data <- function(raw, mapping, treatment_unit = "", time_unit = "min", append_treatment_unit = TRUE, append_time_unit = TRUE) {
  names(raw) <- clean_names(names(raw))

  out <- tibble(
    sample = as.character(raw[[mapping$sample]]),
    concentration_raw = as.character(raw[[mapping$concentration]]),
    time_raw = as.character(raw[[mapping$time]]),
    replicate = as.character(raw[[mapping$replicate]]),
    cfu = suppressWarnings(as.numeric(raw[[mapping$cfu]]))
  ) %>%
    filter(!is.na(sample), !is.na(concentration_raw), !is.na(time_raw), !is.na(cfu), cfu > 0)

  conc_info <- format_label(out$concentration_raw, treatment_unit, append_treatment_unit)
  time_info <- format_label(out$time_raw, time_unit, append_time_unit)

  out %>%
    mutate(
      sample = factor(sample, levels = unique(sample)),
      concentration_value = suppressWarnings(as.numeric(concentration_raw)),
      concentration_label = factor(
        concentration_raw,
        levels = conc_info$values,
        labels = conc_info$labels
      ),
      time_value = suppressWarnings(as.numeric(time_raw)),
      time_min = factor(
        time_raw,
        levels = time_info$values,
        labels = time_info$labels
      ),
      replicate = factor(replicate),
      log10_cfu = log10(cfu)
    )
}

summary_cfu <- function(dat) {
  dat %>%
    group_by(sample, concentration_label, time_min) %>%
    summarize(
      n = n(),
      mean_cfu = mean(cfu),
      sd_cfu = sd(cfu),
      sem_cfu = sd_cfu / sqrt(n),
      mean_log10_cfu = mean(log10_cfu),
      sd_log10_cfu = sd(log10_cfu),
      sem_log10_cfu = sd_log10_cfu / sqrt(n),
      geometric_mean_cfu = 10^mean_log10_cfu,
      .groups = "drop"
    )
}

qc_summary <- function(dat) {
  dat %>%
    group_by(sample, concentration_label, time_min) %>%
    summarize(
      replicates = n_distinct(replicate),
      rows = n(),
      min_cfu = min(cfu, na.rm = TRUE),
      max_cfu = max(cfu, na.rm = TRUE),
      mean_log10_cfu = mean(log10_cfu, na.rm = TRUE),
      sd_log10_cfu = sd(log10_cfu, na.rm = TRUE),
      flag = case_when(
        replicates < 2 ~ "Check: fewer than 2 replicates",
        rows != replicates ~ "Check: duplicate replicate labels",
        TRUE ~ "OK"
      ),
      .groups = "drop"
    )
}

raw_qc_summary <- function(raw, mapping) {
  tibble(
    check = c("Rows in source", "Missing CFU values", "Nonpositive CFU values", "Unique samples", "Unique treatments", "Unique timepoints"),
    value = c(
      nrow(raw),
      sum(is.na(suppressWarnings(as.numeric(raw[[mapping$cfu]])))),
      sum(suppressWarnings(as.numeric(raw[[mapping$cfu]])) <= 0, na.rm = TRUE),
      n_distinct(raw[[mapping$sample]]),
      n_distinct(raw[[mapping$concentration]]),
      n_distinct(raw[[mapping$time]])
    )
  )
}

model_formula <- function(dat) {
  terms <- c("sample", "concentration_label", "time_min")
  usable <- terms[vapply(terms, function(z) n_distinct(dat[[z]]) > 1, logical(1))]
  if (length(usable) == 0) {
    as.formula("log10_cfu ~ 1")
  } else {
    as.formula(paste("log10_cfu ~", paste(usable, collapse = " * ")))
  }
}

run_anova <- function(dat) {
  if (nrow(dat) < 3 || n_distinct(dat$log10_cfu) < 2) return(tibble())
  fit <- lm(model_formula(dat), data = dat)
  tidy(anova(fit)) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 4)))
}

adjust_stats <- function(out, p_adjust, adjustment_scope) {
  if (nrow(out) == 0 || !"p.value" %in% names(out)) return(out)

  if (identical(adjustment_scope, "within_panel") && "panel" %in% names(out)) {
    out <- out %>%
      group_by(panel) %>%
      mutate(q.value = p.adjust(p.value, method = p_adjust)) %>%
      ungroup()
  } else {
    out <- out %>% mutate(q.value = p.adjust(p.value, method = p_adjust))
  }

  out %>%
    mutate(
      significance = significance_label(q.value),
      label_stars = significance,
      label_q = if (identical(p_adjust, "none")) format_p(p.value) else format_q(q.value),
      p_adjust_method = p_adjust,
      adjustment_scope = adjustment_scope
    )
}

run_two_group_test <- function(dat, group_col, level_a, level_b, var_equal) {
  a <- dat %>% filter(.data[[group_col]] == level_a) %>% pull(log10_cfu)
  b <- dat %>% filter(.data[[group_col]] == level_b) %>% pull(log10_cfu)

  if (length(a) < 2 || length(b) < 2) {
    return(tibble(
      p.value = NA_real_,
      statistic = NA_real_,
      parameter = NA_real_,
      estimate = mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE),
      stderr = NA_real_,
      message = "Each group needs at least two replicates for a t-test."
    ))
  }

  test <- tryCatch(
    t.test(a, b, var.equal = var_equal),
    error = function(e) e
  )

  if (inherits(test, "error")) {
    return(tibble(
      p.value = NA_real_,
      statistic = NA_real_,
      parameter = NA_real_,
      estimate = mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE),
      stderr = NA_real_,
      message = conditionMessage(test)
    ))
  }

  tibble(
    p.value = unname(test$p.value),
    statistic = unname(test$statistic),
    parameter = unname(test$parameter),
    estimate = mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE),
    stderr = unname(test$stderr %||% NA_real_),
    message = NA_character_
  )
}

run_groupwise_t_tests <- function(dat, comparison, p_adjust, adjustment_scope, control_concentration, ttest_type) {
  var_equal <- identical(ttest_type, "student")

  if (comparison == "sample") {
    if (n_distinct(dat$sample) < 2) return(tibble(message = "Sample comparison requires at least two samples."))
    levels_to_compare <- levels(droplevels(dat$sample))[seq_len(2)]
    strata <- dat %>% distinct(concentration_label, time_min)
    out <- bind_rows(lapply(seq_len(nrow(strata)), function(i) {
      sub <- dat %>% filter(concentration_label == strata$concentration_label[i], time_min == strata$time_min[i])
      res <- run_two_group_test(sub, "sample", levels_to_compare[1], levels_to_compare[2], var_equal)
      res %>%
        mutate(
          comparison_family = "Sample/vector within treatment and time",
          contrast = paste(levels_to_compare[1], "-", levels_to_compare[2]),
          sample = NA_character_,
          concentration_label = strata$concentration_label[i],
          time_min = strata$time_min[i],
          panel = paste(strata$time_min[i]),
          numerator = levels_to_compare[1],
          denominator = levels_to_compare[2]
        )
    }))
  } else if (comparison == "time") {
    if (n_distinct(dat$time_min) < 2) return(tibble(message = "Timepoint comparison requires at least two timepoints."))
    levels_to_compare <- levels(droplevels(dat$time_min))[seq_len(2)]
    strata <- dat %>% distinct(sample, concentration_label)
    out <- bind_rows(lapply(seq_len(nrow(strata)), function(i) {
      sub <- dat %>% filter(sample == strata$sample[i], concentration_label == strata$concentration_label[i])
      res <- run_two_group_test(sub, "time_min", levels_to_compare[1], levels_to_compare[2], var_equal)
      res %>%
        mutate(
          comparison_family = "Timepoints within sample/vector and treatment",
          contrast = paste(levels_to_compare[1], "-", levels_to_compare[2]),
          sample = as.character(strata$sample[i]),
          concentration_label = strata$concentration_label[i],
          time_min = NA_character_,
          panel = paste(strata$sample[i]),
          numerator = levels_to_compare[1],
          denominator = levels_to_compare[2]
        )
    }))
  } else if (comparison == "concentration_vs_control") {
    if (n_distinct(dat$concentration_label) < 2) return(tibble(message = "Treatment comparison requires at least two treatment groups."))
    if (!control_concentration %in% levels(droplevels(dat$concentration_label))) {
      return(tibble(message = "The selected control treatment is not present in the filtered data."))
    }
    strata <- dat %>% distinct(sample, time_min)
    out <- bind_rows(lapply(seq_len(nrow(strata)), function(i) {
      sub <- dat %>% filter(sample == strata$sample[i], time_min == strata$time_min[i])
      test_levels <- setdiff(levels(droplevels(sub$concentration_label)), control_concentration)
      bind_rows(lapply(test_levels, function(lvl) {
        res <- run_two_group_test(sub, "concentration_label", lvl, control_concentration, var_equal)
        res %>%
          mutate(
            comparison_family = "Treatment versus control within sample/vector and time",
            contrast = paste(lvl, "-", control_concentration),
            sample = as.character(strata$sample[i]),
            concentration_label = lvl,
            time_min = as.character(strata$time_min[i]),
            panel = paste(strata$sample[i], strata$time_min[i]),
            numerator = lvl,
            denominator = control_concentration
          )
      }))
    }))
  } else if (comparison == "concentration_all") {
    if (n_distinct(dat$concentration_label) < 2) return(tibble(message = "Treatment comparison requires at least two treatment groups."))
    strata <- dat %>% distinct(sample, time_min)
    out <- bind_rows(lapply(seq_len(nrow(strata)), function(i) {
      sub <- dat %>% filter(sample == strata$sample[i], time_min == strata$time_min[i])
      levs <- levels(droplevels(sub$concentration_label))
      pairs <- combn(levs, 2, simplify = FALSE)
      bind_rows(lapply(pairs, function(pair) {
        res <- run_two_group_test(sub, "concentration_label", pair[1], pair[2], var_equal)
        res %>%
          mutate(
            comparison_family = "All treatment pairs within sample/vector and time",
            contrast = paste(pair[1], "-", pair[2]),
            sample = as.character(strata$sample[i]),
            concentration_label = pair[1],
            time_min = as.character(strata$time_min[i]),
            panel = paste(strata$sample[i], strata$time_min[i]),
            numerator = pair[1],
            denominator = pair[2]
          )
      }))
    }))
  } else {
    return(tibble())
  }

  adjust_stats(out, p_adjust, adjustment_scope) %>%
    mutate(
      test = if (var_equal) "Student t-test on log10(CFU)" else "Welch t-test on log10(CFU)",
      estimate_log10_difference = estimate,
      fold_change = 10^estimate
    ) %>%
    select(
      comparison_family, test, contrast, sample, concentration_label, time_min,
      estimate_log10_difference, fold_change, statistic, parameter, p.value, q.value,
      significance, p_adjust_method, adjustment_scope, message, everything()
    )
}

run_contrast <- function(dat, comparison, p_adjust, control_concentration) {
  if (nrow(dat) < 3 || n_distinct(dat$log10_cfu) < 2) return(tibble())
  if (comparison == "sample" && n_distinct(dat$sample) < 2) return(tibble(message = "Sample comparison requires at least two samples."))
  if (comparison == "time" && n_distinct(dat$time_min) < 2) return(tibble(message = "Timepoint comparison requires at least two timepoints."))
  if (comparison %in% c("concentration_vs_control", "concentration_all") && n_distinct(dat$concentration_label) < 2) {
    return(tibble(message = "Treatment comparison requires at least two treatment groups."))
  }

  fit <- lm(model_formula(dat), data = dat)

  result <- tryCatch({
    if (comparison == "sample") {
      by_vars <- c("concentration_label", "time_min")
      by_vars <- by_vars[vapply(by_vars, function(z) n_distinct(dat[[z]]) > 1, logical(1))]
      spec <- if (length(by_vars) > 0) {
        as.formula(paste("~ sample |", paste(by_vars, collapse = " * ")))
      } else {
        ~ sample
      }
      pairs(emmeans(fit, spec), adjust = "none")
    } else if (comparison == "time") {
      by_vars <- c("sample", "concentration_label")
      by_vars <- by_vars[vapply(by_vars, function(z) n_distinct(dat[[z]]) > 1, logical(1))]
      spec <- if (length(by_vars) > 0) {
        as.formula(paste("~ time_min |", paste(by_vars, collapse = " * ")))
      } else {
        ~ time_min
      }
      pairs(emmeans(fit, spec), adjust = "none")
    } else if (comparison == "concentration_vs_control") {
      by_vars <- c("sample", "time_min")
      by_vars <- by_vars[vapply(by_vars, function(z) n_distinct(dat[[z]]) > 1, logical(1))]
      spec <- if (length(by_vars) > 0) {
        as.formula(paste("~ concentration_label |", paste(by_vars, collapse = " * ")))
      } else {
        ~ concentration_label
      }
      emm <- emmeans(fit, spec)
      emm_df <- as.data.frame(emm)
      control_idx <- which(as.character(emm_df$concentration_label) == control_concentration)[1]
      if (is.na(control_idx)) {
        stop("The selected control treatment is not present in the data.", call. = FALSE)
      }
      contrast(emm, method = "trt.vs.ctrl", ref = control_idx, adjust = "none")
    } else {
      by_vars <- c("sample", "time_min")
      by_vars <- by_vars[vapply(by_vars, function(z) n_distinct(dat[[z]]) > 1, logical(1))]
      spec <- if (length(by_vars) > 0) {
        as.formula(paste("~ concentration_label |", paste(by_vars, collapse = " * ")))
      } else {
        ~ concentration_label
      }
      pairs(emmeans(fit, spec), adjust = "none")
    }
  }, error = function(e) {
    tibble(message = conditionMessage(e))
  })

  out <- as_tibble(as.data.frame(result))
  if (nrow(out) == 0) return(out)
  if (!"p.value" %in% names(out)) return(out)

  adjust_stats(out, p_adjust, "global") %>%
    mutate(
      test = "Linear model + emmeans on log10(CFU)",
      estimate_log10_difference = estimate,
      fold_change = 10^estimate
    )
}

plot_summary <- function(dat, plot_mode, y_mode, error_type) {
  group_cols <- switch(
    plot_mode,
    combined = c("concentration_label", "sample", "time_min"),
    sample_both = c("concentration_label", "time_min"),
    sample_time = c("concentration_label"),
    c("concentration_label", "sample", "time_min")
  )

  dat %>%
    mutate(plot_y = if (y_mode == "log10") log10_cfu else cfu) %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      n = n(),
      mean_y = mean(plot_y, na.rm = TRUE),
      median_y = median(plot_y, na.rm = TRUE),
      sd_y = sd(plot_y, na.rm = TRUE),
      sem_y = sd_y / sqrt(n),
      q1_y = quantile(plot_y, 0.25, na.rm = TRUE, names = FALSE),
      q3_y = quantile(plot_y, 0.75, na.rm = TRUE, names = FALSE),
      min_y = min(plot_y, na.rm = TRUE),
      max_y = max(plot_y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      err_y = case_when(
        error_type == "SD" ~ sd_y,
        error_type == "SEM" ~ sem_y,
        error_type == "95% CI" ~ qt(0.975, pmax(n - 1, 1)) * sem_y,
        TRUE ~ sd_y
      ),
      ymin = case_when(
        error_type == "IQR" ~ q1_y,
        error_type == "Range (min-max)" ~ min_y,
        TRUE ~ mean_y - err_y
      ),
      ymax = case_when(
        error_type == "IQR" ~ q3_y,
        error_type == "Range (min-max)" ~ max_y,
        TRUE ~ mean_y + err_y
      ),
      ymin = pmax(ymin, if (y_mode == "raw_log_axis") .Machine$double.eps else 0)
    )
}

annotation_data <- function(stats, sumdat, comparison, plot_mode, label_kind, show_ns, y_mode) {
  if (nrow(stats) == 0) return(tibble())
  if (!all(c("label_stars", "label_q", "p.value") %in% names(stats))) return(tibble())

  label_col <- if (label_kind == "stars") "label_stars" else "label_q"
  stats <- stats %>%
    mutate(label = .data[[label_col]])

  if (!isTRUE(show_ns)) {
    stats <- stats %>% filter(label != "ns")
  }

  if (nrow(stats) == 0) return(tibble())

  pad <- if (y_mode == "raw_log_axis") 0.25 * max(sumdat$ymax, na.rm = TRUE) else 0.45

  if (comparison == "sample") {
    yref <- sumdat %>%
      group_by(concentration_label, time_min) %>%
      summarize(y = max(ymax, na.rm = TRUE) + pad, .groups = "drop")
    stats %>% left_join(yref, by = c("concentration_label", "time_min")) %>%
      mutate(x = concentration_label)
  } else if (comparison == "time") {
    yref <- sumdat %>%
      group_by(concentration_label) %>%
      summarize(y = max(ymax, na.rm = TRUE) + pad, .groups = "drop")
    stats %>% left_join(yref, by = "concentration_label") %>%
      mutate(x = concentration_label)
  } else if (comparison == "concentration_vs_control") {
    stats <- stats %>%
      mutate(concentration_label = sub(" - .*", "", contrast))
    yref <- sumdat %>%
      group_by(concentration_label) %>%
      summarize(y = max(ymax, na.rm = TRUE) + pad, .groups = "drop")
    stats %>% left_join(yref, by = "concentration_label") %>%
      mutate(x = concentration_label)
  } else {
    tibble()
  }
}

plot_group_cols <- function(plot_mode) {
  switch(
    plot_mode,
    combined = c("time_min", "concentration_label", "sample"),
    sample_both = c("concentration_label", "time_min"),
    sample_time = c("concentration_label"),
    c("time_min", "concentration_label", "sample")
  )
}

reveal_data <- function(dat, sumdat, plot_mode) {
  group_cols <- plot_group_cols(plot_mode)
  groups <- sumdat %>%
    distinct(across(all_of(group_cols))) %>%
    arrange(across(all_of(group_cols))) %>%
    mutate(reveal_order = row_number())

  list(
    groups = groups,
    sumdat = sumdat %>% left_join(groups, by = group_cols),
    dat = dat %>% inner_join(groups, by = group_cols)
  )
}

expand_reveal <- function(x, total_steps) {
  bind_rows(lapply(seq_len(total_steps), function(step) {
    x %>%
      filter(reveal_order <= step) %>%
      mutate(frame = step)
  }))
}

make_cfu_plot <- function(dat, sumdat, ann, plot_mode, y_mode, error_type, input) {
  y_lab <- if (y_mode == "log10") expression(log[10]~"CFU") else "CFU"
  y_min <- axis_limit(input$y_min)
  y_max <- axis_limit(input$y_max)
  if (y_mode == "raw_log_axis" && !is.na(y_min) && y_min <= 0) {
    y_min <- NA_real_
  }
  if (y_mode == "raw_log_axis" && !is.na(y_max) && y_max <= 0) {
    y_max <- NA_real_
  }
  has_y_limits <- !is.na(y_min) || !is.na(y_max)
  coord_limits <- c(y_min, y_max)
  subtitle_text <- if (isTRUE(input$show_subtitle)) input$plot_subtitle else NULL
  if (identical(input$comparison, "none") && isTRUE(input$hide_subtitle_no_stats)) {
    subtitle_text <- NULL
  }
  plot_theme <- input$plot_theme %||% "classic"
  axis_col <- input$axis_color %||% "grey15"
  grid_col <- input$grid_color %||% "grey87"
  bar_outline_col <- input$bar_outline_color %||% "grey20"
  bar_outline_lwd <- input$bar_outline_width %||% 0.25
  errorbar_lwd <- input$errorbar_width %||% 0.55
  stat_col <- input$stat_color %||% "grey15"
  variation_display <- input$variation_display %||% "errorbar"

  # Auto methods caption: names error-bar type, and (when stats are shown) the
  # test + multiple-comparison correction. User can disable via the sidebar.
  caption_text <- NULL
  if (isTRUE(input$show_method_caption %||% TRUE)) {
    caption_parts <- c(error_type_caption(error_type, variation_display))
    stats_on <- !identical(input$comparison %||% "auto", "none")
    if (stats_on) {
      caption_parts <- c(caption_parts, stats_caption(input$stats_method, input$p_adjust))
    }
    caption_parts <- caption_parts[nzchar(caption_parts %||% "")]
    if (length(caption_parts) > 0) {
      caption_text <- paste(caption_parts, collapse = "; ")
      substr(caption_text, 1, 1) <- toupper(substr(caption_text, 1, 1))
      caption_text <- paste0(caption_text, ".")
    }
  }

  finite_y <- range(c(sumdat$ymin, sumdat$ymax), finite = TRUE)
  if (!all(is.finite(finite_y))) finite_y <- c(0, 1)
  break_min <- if (!is.na(y_min)) y_min else finite_y[1]
  break_max <- if (!is.na(y_max)) y_max else finite_y[2]
  major_breaks <- scale_breaks_or_default(axis_step_breaks(break_min, break_max, input$y_major_step, log_base_10 = identical(y_mode, "raw_log_axis")))
  minor_breaks <- scale_breaks_or_default(axis_step_breaks(break_min, break_max, input$y_minor_step, log_base_10 = identical(y_mode, "raw_log_axis")))
  use_minor_ticks <- isTRUE(input$show_minor_y_ticks)
  y_guide <- if ("minor.ticks" %in% names(formals(ggplot2::guide_axis))) {
    ggplot2::guide_axis(minor.ticks = use_minor_ticks)
  } else {
    ggplot2::guide_axis()
  }
  legend_pos <- input$legend_position %||% "top"
  legend_theme <- if (identical(legend_pos, "inside")) {
    theme(
      legend.position = "inside",
      legend.position.inside = c(input$legend_x %||% 0.98, input$legend_y %||% 0.98),
      legend.justification = c(input$legend_just_x %||% 1, input$legend_just_y %||% 1),
      legend.background = element_rect(fill = grDevices::adjustcolor("white", alpha.f = 0.86), color = "grey75", linewidth = 0.25)
    )
  } else {
    theme(legend.position = legend_pos)
  }

  base_theme <- theme_classic(base_size = input$font_size) +
    theme(
      plot.title = element_text(face = "bold", size = input$title_size),
      plot.subtitle = element_text(size = input$subtitle_size, color = "grey30"),
      axis.text.x = element_text(angle = input$x_angle, hjust = 1),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text = element_text(face = "bold"),
      axis.ticks.y = if (isTRUE(input$show_y_ticks)) element_line(color = axis_col, linewidth = input$axis_line_width) else element_blank(),
      axis.minor.ticks.y.left = if (use_minor_ticks) element_line(color = axis_col, linewidth = input$axis_line_width * 0.75) else element_blank(),
      axis.ticks.length.y = grid::unit(input$y_tick_length, "pt"),
      axis.minor.ticks.length.y = grid::unit(input$minor_y_tick_length %||% 2, "pt"),
      axis.line = element_line(color = axis_col, linewidth = input$axis_line_width),
      panel.grid.major.y = if (isTRUE(input$show_y_grid)) element_line(color = grid_col, linewidth = 0.3) else element_blank(),
      panel.grid.minor.y = if (isTRUE(input$show_minor_y_grid)) element_line(color = grid_col, linewidth = 0.18) else element_blank(),
      panel.border = if (isTRUE(input$plot_box)) element_rect(color = axis_col, fill = NA, linewidth = input$box_line_width) else element_blank()
    ) +
    legend_theme

  if (identical(plot_theme, "minimal_grid")) {
    base_theme <- theme_minimal(base_size = input$font_size) +
      theme(
        plot.title = element_text(face = "bold", size = input$title_size),
        plot.subtitle = element_text(size = input$subtitle_size, color = "grey30"),
        axis.text.x = element_text(angle = input$x_angle, hjust = 1),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3),
        panel.grid.minor.y = if (isTRUE(input$show_minor_y_grid)) element_line(color = grid_col, linewidth = 0.18) else element_blank(),
        axis.line = element_line(color = axis_col, linewidth = input$axis_line_width),
        axis.ticks.y = if (isTRUE(input$show_y_ticks)) element_line(color = axis_col, linewidth = input$axis_line_width) else element_blank(),
        axis.minor.ticks.y.left = if (use_minor_ticks) element_line(color = axis_col, linewidth = input$axis_line_width * 0.75) else element_blank(),
        axis.ticks.length.y = grid::unit(input$y_tick_length, "pt"),
        axis.minor.ticks.length.y = grid::unit(input$minor_y_tick_length %||% 2, "pt"),
        panel.border = if (isTRUE(input$plot_box)) element_rect(color = axis_col, fill = NA, linewidth = input$box_line_width) else element_blank()
      ) +
      legend_theme
  } else if (identical(plot_theme, "boxed")) {
    base_theme <- base_theme +
      theme(
        panel.border = element_rect(color = axis_col, fill = NA, linewidth = input$box_line_width),
        axis.line = element_blank()
      )
  }

  add_interval_layer <- function(plot_obj, position_obj, width = 0.22) {
    if (identical(variation_display, "none")) return(plot_obj)
    if (identical(variation_display, "linerange")) {
      return(plot_obj + geom_linerange(
        aes(ymin = ymin, ymax = ymax),
        position = position_obj,
        linewidth = errorbar_lwd,
        color = axis_col,
        show.legend = FALSE
      ))
    }
    if (identical(variation_display, "pointrange")) {
      return(plot_obj + geom_pointrange(
        aes(y = mean_y, ymin = ymin, ymax = ymax),
        position = position_obj,
        linewidth = errorbar_lwd,
        size = input$point_size * 0.75,
        color = axis_col,
        show.legend = FALSE
      ))
    }
    if (identical(variation_display, "crossbar")) {
      return(plot_obj + geom_crossbar(
        aes(y = mean_y, ymin = ymin, ymax = ymax),
        position = position_obj,
        width = width * 1.35,
        linewidth = errorbar_lwd,
        color = axis_col,
        fill = NA,
        show.legend = FALSE
      ))
    }
    plot_obj + geom_errorbar(
      aes(ymin = ymin, ymax = ymax),
      position = position_obj,
      width = width,
      linewidth = errorbar_lwd,
      color = axis_col,
      show.legend = FALSE
    )
  }

  if (plot_mode == "combined") {
    sample_colors <- named_palette(levels(dat$sample), c(input$sample_color_1, input$sample_color_2))
    dodge_pos <- position_dodge(width = input$dodge_width)
    p <- ggplot(sumdat, aes(x = concentration_label, y = mean_y, fill = sample)) +
      geom_col(position = dodge_pos, width = input$bar_width, color = bar_outline_col, linewidth = bar_outline_lwd)
    p <- add_interval_layer(p, dodge_pos, width = 0.22)
    if (isTRUE(input$show_points)) {
      p <- p + geom_point(
        data = dat,
        aes(x = concentration_label, y = if (y_mode == "log10") log10_cfu else cfu, fill = sample),
        position = position_jitterdodge(jitter.width = input$jitter_width, dodge.width = input$dodge_width),
        shape = 21, size = input$point_size, color = bar_outline_col, stroke = 0.25, alpha = input$point_alpha
      )
    }
    p <- p +
      facet_wrap(~ time_min, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = sample_colors) +
      labs(x = input$x_label, y = y_lab, fill = input$legend_title, title = input$plot_title, subtitle = subtitle_text)
  } else if (plot_mode == "sample_both") {
    time_colors <- named_palette(levels(dat$time_min), c(input$time_color_1, input$time_color_2))
    dodge_pos <- position_dodge(width = input$dodge_width)
    p <- ggplot(sumdat, aes(x = concentration_label, y = mean_y, fill = time_min)) +
      geom_col(position = dodge_pos, width = input$bar_width, color = bar_outline_col, linewidth = bar_outline_lwd)
    p <- add_interval_layer(p, dodge_pos, width = 0.22)
    if (isTRUE(input$show_points)) {
      p <- p + geom_point(
        data = dat,
        aes(x = concentration_label, y = if (y_mode == "log10") log10_cfu else cfu, fill = time_min),
        position = position_jitterdodge(jitter.width = input$jitter_width, dodge.width = input$dodge_width),
        shape = 21, size = input$point_size, color = bar_outline_col, stroke = 0.25, alpha = input$point_alpha
      )
    }
    p <- p +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = time_colors) +
      labs(x = input$x_label, y = y_lab, fill = input$legend_title, title = input$plot_title, subtitle = subtitle_text)
  } else {
    p <- ggplot(sumdat, aes(x = concentration_label, y = mean_y)) +
      geom_col(width = input$bar_width, fill = input$single_color, color = bar_outline_col, linewidth = bar_outline_lwd)
    p <- add_interval_layer(p, position_identity(), width = 0.2)
    if (isTRUE(input$show_points)) {
      p <- p + geom_point(
        data = dat,
        aes(x = concentration_label, y = if (y_mode == "log10") log10_cfu else cfu),
        position = position_jitter(width = input$jitter_width, height = 0),
        shape = 21, size = input$point_size, fill = input$single_color, color = bar_outline_col, stroke = 0.25, alpha = input$point_alpha
      )
    }
    p <- p +
      scale_x_discrete(drop = FALSE) +
      labs(x = input$x_label, y = y_lab, title = input$plot_title, subtitle = subtitle_text)
  }

  if (nrow(ann) > 0) {
    p <- p + geom_text(data = ann, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = input$stat_size, color = stat_col)
  }

  if (!is.null(caption_text)) {
    p <- p + labs(caption = caption_text)
  }

  p <- p + base_theme +
    theme(plot.caption = element_text(size = (input$subtitle_size %||% 10) * 0.92, color = "grey35", hjust = 0))

  if (y_mode == "raw_log_axis") {
    p <- p + scale_y_log10(breaks = major_breaks, minor_breaks = minor_breaks, guide = y_guide)
    if (identical(input$bar_orientation, "horizontal")) {
      p <- if (has_y_limits) {
        p + coord_flip(ylim = coord_limits, clip = "off")
      } else {
        p + coord_flip(clip = "off")
      }
    } else if (has_y_limits) {
      p <- p + coord_cartesian(ylim = coord_limits, clip = "off")
    }
  } else {
    p <- p + scale_y_continuous(breaks = major_breaks, minor_breaks = minor_breaks, guide = y_guide, expand = expansion(mult = c(0, 0.08)))
    if (identical(input$bar_orientation, "horizontal")) {
      p <- if (has_y_limits) {
        p + coord_flip(ylim = coord_limits, clip = "off")
      } else {
        p + coord_flip(clip = "off")
      }
    } else if (has_y_limits) {
      p <- p + coord_cartesian(ylim = coord_limits, clip = "off")
    }
  }

  p
}

make_reveal_plot <- function(dat, sumdat, plot_mode, y_mode, error_type, input, step = NULL) {
  reveal <- reveal_data(dat, sumdat, plot_mode)
  if (!is.null(step)) {
    step <- max(1, min(step, nrow(reveal$groups)))
    dat <- reveal$dat %>% filter(reveal_order <= step)
    sumdat <- reveal$sumdat %>% filter(reveal_order <= step)
  }

  make_cfu_plot(
    dat = dat,
    sumdat = sumdat,
    ann = tibble(),
    plot_mode = plot_mode,
    y_mode = y_mode,
    error_type = error_type,
    input = input
  )
}

make_animated_cfu_plot <- function(dat, sumdat, plot_mode, y_mode, error_type, input) {
  validate(need(requireNamespace("gganimate", quietly = TRUE), "Package gganimate is required for GIF export."))
  validate(need(requireNamespace("gifski", quietly = TRUE), "Package gifski is required for GIF export."))

  reveal <- reveal_data(dat, sumdat, plot_mode)
  total_steps <- nrow(reveal$groups)
  validate(need(total_steps > 0, "No bars are available to animate."))

  sum_frames <- expand_reveal(reveal$sumdat, total_steps)
  dat_frames <- expand_reveal(reveal$dat, total_steps)

  p <- make_cfu_plot(
    dat = dat_frames,
    sumdat = sum_frames,
    ann = tibble(),
    plot_mode = plot_mode,
    y_mode = y_mode,
    error_type = error_type,
    input = input
  ) +
    gganimate::transition_manual(frame) +
    labs(caption = "Reveal step {current_frame}")

  list(plot = p, steps = total_steps)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {
      background:
        radial-gradient(circle at 18% 8%, rgba(122, 166, 194, 0.18), transparent 26%),
        radial-gradient(circle at 88% 16%, rgba(228, 87, 86, 0.10), transparent 22%),
        linear-gradient(180deg, #f7fbfa 0%, #f5f6f4 48%, #ffffff 100%);
      color: #1f2a2a;
    }
    .container-fluid { max-width: 1540px; }
    .well {
      background: rgba(255, 255, 255, 0.94);
      border: 1px solid rgba(42, 73, 77, 0.12);
      border-radius: 8px;
      box-shadow: 0 12px 30px rgba(34, 49, 52, 0.08);
    }
    .tab-content {
      background: rgba(255, 255, 255, 0.82);
      border: 1px solid rgba(42, 73, 77, 0.10);
      border-top: 0;
      padding: 0 14px 18px;
      box-shadow: 0 12px 30px rgba(34, 49, 52, 0.06);
    }
    .nav-tabs > li > a {
      border-radius: 7px 7px 0 0;
      color: #31535a;
      font-weight: 600;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      color: #14363d;
      border-color: rgba(42, 73, 77, 0.14);
      border-bottom-color: transparent;
    }
    .btn {
      border-radius: 6px;
      font-weight: 600;
      border-color: rgba(42, 73, 77, 0.18);
    }
    .btn-default {
      background: #ffffff;
      color: #22474f;
    }
    .btn-default:hover,
    .btn-default:focus {
      background: #eef7f5;
      border-color: rgba(42, 73, 77, 0.28);
      color: #14363d;
    }
    .form-control {
      border-radius: 6px;
      border-color: rgba(42, 73, 77, 0.18);
      box-shadow: none;
    }
    .form-control:focus {
      border-color: #6aa6a1;
      box-shadow: 0 0 0 3px rgba(106, 166, 161, 0.14);
    }
    .lab-hero {
      position: relative;
      overflow: hidden;
      display: flex;
      justify-content: space-between;
      gap: 28px;
      align-items: center;
      margin: 18px 0 18px;
      padding: 28px 32px;
      background:
        linear-gradient(135deg, rgba(255, 255, 255, 0.98), rgba(236, 246, 243, 0.94)),
        linear-gradient(90deg, rgba(80, 126, 132, 0.08), rgba(198, 90, 82, 0.08));
      border: 1px solid rgba(43, 72, 76, 0.14);
      border-radius: 8px;
      box-shadow: 0 18px 45px rgba(34, 49, 52, 0.10);
    }
    .lab-hero:before {
      content: '';
      position: absolute;
      inset: auto -60px -90px auto;
      width: 270px;
      height: 270px;
      background: radial-gradient(circle, rgba(106, 166, 161, 0.18), transparent 68%);
      pointer-events: none;
    }
    .hero-copy { max-width: 820px; position: relative; z-index: 1; }
    .hero-kicker {
      margin-bottom: 8px;
      color: #58737b;
      font-size: 12px;
      font-weight: 800;
      letter-spacing: 0.08em;
      text-transform: uppercase;
    }
    .app-title {
      margin: 0 0 8px;
      color: #132f36;
      font-size: 34px;
      line-height: 1.12;
      font-weight: 800;
      letter-spacing: 0;
    }
    .app-subtitle {
      max-width: 760px;
      color: #415d61;
      margin: 0 0 15px;
      font-size: 15px;
      line-height: 1.5;
    }
    .hero-badges {
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
    }
    .badge-pill {
      display: inline-flex;
      align-items: center;
      gap: 7px;
      padding: 7px 10px;
      border-radius: 999px;
      background: #ffffff;
      border: 1px solid rgba(42, 73, 77, 0.12);
      color: #31535a;
      font-size: 12px;
      font-weight: 700;
      box-shadow: 0 6px 16px rgba(34, 49, 52, 0.05);
    }
    .badge-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background: #6aa6a1;
      box-shadow: 0 0 0 3px rgba(106, 166, 161, 0.16);
    }
    .badge-dot.red { background: #d66b63; box-shadow: 0 0 0 3px rgba(214, 107, 99, 0.16); }
    .badge-dot.blue { background: #4c78a8; box-shadow: 0 0 0 3px rgba(76, 120, 168, 0.16); }
    .petri-stage {
      width: 230px;
      min-width: 230px;
      display: flex;
      justify-content: center;
      position: relative;
      z-index: 1;
    }
    .petri-dish {
      position: relative;
      width: 182px;
      height: 182px;
      border-radius: 50%;
      background:
        radial-gradient(circle at 35% 28%, rgba(255, 255, 255, 0.92), rgba(229, 243, 239, 0.9) 45%, rgba(198, 225, 219, 0.82) 100%);
      border: 8px solid rgba(255, 255, 255, 0.9);
      box-shadow:
        inset 0 0 0 2px rgba(53, 91, 94, 0.18),
        inset 16px 18px 38px rgba(255, 255, 255, 0.72),
        0 22px 36px rgba(31, 55, 57, 0.18);
    }
    .petri-dish:before {
      content: '';
      position: absolute;
      inset: 20px 26px auto auto;
      width: 54px;
      height: 18px;
      border-radius: 50%;
      background: rgba(255, 255, 255, 0.55);
      transform: rotate(-24deg);
    }
    .petri-ring {
      position: absolute;
      inset: 18px;
      border-radius: 50%;
      border: 1px dashed rgba(49, 83, 90, 0.18);
    }
    .colony {
      position: absolute;
      width: var(--s);
      height: var(--s);
      left: var(--x);
      top: var(--y);
      border-radius: 50%;
      background: var(--c);
      box-shadow: 0 2px 7px rgba(33, 55, 57, 0.14);
      animation: colonyPulse 3.8s ease-in-out infinite;
      animation-delay: var(--d);
    }
    @keyframes colonyPulse {
      0%, 100% { transform: scale(1); opacity: 0.92; }
      50% { transform: scale(1.16); opacity: 1; }
    }
    .lab-overview {
      margin: 2px 0 14px;
      padding: 15px;
      background: linear-gradient(135deg, rgba(255, 255, 255, 0.98), rgba(246, 250, 249, 0.94));
      border: 1px solid rgba(42, 73, 77, 0.12);
      border-radius: 8px;
    }
    .overview-top {
      display: flex;
      justify-content: space-between;
      gap: 12px;
      align-items: center;
      margin-bottom: 12px;
    }
    .overview-title {
      color: #183a41;
      font-size: 15px;
      font-weight: 800;
    }
    .source-chip {
      display: inline-flex;
      align-items: center;
      padding: 6px 9px;
      border-radius: 999px;
      background: #eef7f5;
      color: #31535a;
      border: 1px solid rgba(42, 73, 77, 0.12);
      font-size: 12px;
      font-weight: 700;
    }
    .metric-row {
      display: grid;
      grid-template-columns: repeat(4, minmax(120px, 1fr));
      gap: 10px;
    }
    .metric-card {
      position: relative;
      overflow: hidden;
      min-height: 88px;
      padding: 13px 14px;
      background: #ffffff;
      border: 1px solid rgba(42, 73, 77, 0.10);
      border-radius: 8px;
      box-shadow: 0 8px 18px rgba(34, 49, 52, 0.06);
    }
    .metric-card:after {
      content: '';
      position: absolute;
      right: -18px;
      bottom: -22px;
      width: 72px;
      height: 72px;
      border-radius: 50%;
      background: radial-gradient(circle, rgba(106, 166, 161, 0.18), transparent 66%);
    }
    .metric-value {
      display: block;
      color: #14363d;
      font-size: 25px;
      line-height: 1.1;
      font-weight: 800;
    }
    .metric-label {
      display: block;
      margin-top: 4px;
      color: #607579;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
    }
    .metric-note {
      display: block;
      margin-top: 6px;
      color: #597071;
      font-size: 12px;
    }
    .lab-tip {
      margin-top: 12px;
      padding: 10px 12px;
      border-radius: 7px;
      font-weight: 600;
      border: 1px solid rgba(42, 73, 77, 0.10);
    }
    .lab-tip.ok {
      background: #eff8f1;
      color: #2e6b35;
    }
    .lab-tip.warn {
      background: #fff8e8;
      color: #806126;
    }
    .bench-strip {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 12px;
      margin: -2px 0 18px;
    }
    .bench-card {
      position: relative;
      overflow: hidden;
      min-height: 112px;
      padding: 15px 15px 14px 92px;
      background: rgba(255, 255, 255, 0.90);
      border: 1px solid rgba(42, 73, 77, 0.12);
      border-radius: 8px;
      box-shadow: 0 10px 24px rgba(34, 49, 52, 0.07);
    }
    .bench-title {
      display: block;
      color: #17383f;
      font-size: 14px;
      font-weight: 800;
      margin-bottom: 4px;
    }
    .bench-text {
      color: #536b6f;
      font-size: 12px;
      line-height: 1.42;
      margin: 0;
    }
    .bench-art {
      position: absolute;
      left: 16px;
      top: 18px;
      width: 58px;
      height: 70px;
    }
    .pipette {
      position: absolute;
      width: 12px;
      height: 58px;
      left: 18px;
      top: 2px;
      border-radius: 7px;
      background: linear-gradient(#31535a, #6aa6a1);
      transform: rotate(34deg);
      box-shadow: 18px 18px 0 -5px rgba(214, 107, 99, 0.72);
    }
    .pipette:after {
      content: '';
      position: absolute;
      left: 2px;
      bottom: -10px;
      width: 8px;
      height: 14px;
      border-radius: 50% 50% 55% 55%;
      background: #4c78a8;
    }
    .mini-plate {
      position: absolute;
      left: 4px;
      top: 6px;
      width: 54px;
      height: 54px;
      border-radius: 50%;
      background: #eef7f5;
      border: 5px solid #ffffff;
      box-shadow: inset 0 0 0 1px rgba(49, 83, 90, 0.18), 0 9px 18px rgba(34, 49, 52, 0.12);
    }
    .mini-plate span {
      position: absolute;
      width: 7px;
      height: 7px;
      border-radius: 50%;
      background: var(--c);
      left: var(--x);
      top: var(--y);
    }
    .mini-bars {
      position: absolute;
      left: 8px;
      bottom: 7px;
      display: flex;
      align-items: end;
      gap: 6px;
      width: 50px;
      height: 58px;
    }
    .mini-bars span {
      width: 9px;
      height: var(--h);
      border-radius: 3px 3px 0 0;
      background: var(--c);
      box-shadow: 0 4px 9px rgba(34, 49, 52, 0.10);
    }
    .figure-guide {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 10px;
      margin: 0 0 14px;
    }
    .guide-card {
      min-height: 82px;
      padding: 12px 13px;
      background: #ffffff;
      border: 1px solid rgba(42, 73, 77, 0.10);
      border-radius: 8px;
      box-shadow: 0 7px 18px rgba(34, 49, 52, 0.05);
    }
    .guide-label {
      display: block;
      color: #17383f;
      font-size: 12px;
      font-weight: 800;
      letter-spacing: 0.05em;
      text-transform: uppercase;
      margin-bottom: 5px;
    }
    .guide-text {
      color: #536b6f;
      font-size: 12px;
      line-height: 1.42;
    }
    .copy-status { color: #36613a; display: inline-block; margin-left: 8px; min-height: 20px; }
    .qc-ok { color: #2e6b35; font-weight: 600; }
    @media (max-width: 980px) {
      .lab-hero { align-items: flex-start; }
      .petri-stage { width: 170px; min-width: 170px; }
      .petri-dish { width: 145px; height: 145px; }
      .metric-row { grid-template-columns: repeat(2, minmax(120px, 1fr)); }
      .bench-strip, .figure-guide { grid-template-columns: 1fr; }
    }
    @media (max-width: 720px) {
      .lab-hero { display: block; padding: 22px; }
      .petri-stage { margin: 18px auto 0; }
      .metric-row { grid-template-columns: 1fr; }
      .overview-top { display: block; }
      .source-chip { margin-top: 8px; }
      .bench-card { padding-left: 84px; }
    }
  ")),
  tags$script(HTML("
    document.addEventListener('click', async function(event) {
      const button = event.target.closest('#copy_plot');
      if (!button) return;
      const status = document.getElementById('copy_plot_status');
      const setStatus = (msg, isError) => {
        if (!status) return;
        status.textContent = msg;
        status.style.color = isError ? '#9b2d20' : '#36613a';
      };
      try {
        const img = document.querySelector('#cfu_plot img');
        if (!img || !img.src) {
          setStatus('No plot image is ready yet.', true);
          return;
        }
        if (!navigator.clipboard || !window.ClipboardItem) {
          setStatus('Clipboard image copy is not supported in this browser.', true);
          return;
        }
        const response = await fetch(img.src);
        const blob = await response.blob();
        const pngBlob = blob.type === 'image/png' ? blob : new Blob([blob], { type: 'image/png' });
        await navigator.clipboard.write([new ClipboardItem({ 'image/png': pngBlob })]);
        setStatus('Copied current plot to clipboard.', false);
      } catch (err) {
        setStatus('Copy failed. Try the PNG download instead.', true);
      }
    });
  "))),
  div(
    class = "lab-hero",
    div(
      class = "hero-copy",
      div(class = "hero-kicker", "Lab figure utility"),
      h1(class = "app-title", "CFU Plot Studio"),
      p(
        class = "app-subtitle",
        "Upload replicate-level CFU data, run log10(CFU) statistics, customize publication plots, and export figures, tables, GIFs, and PowerPoint slides."
      ),
      div(
        class = "hero-badges",
        span(class = "badge-pill", span(class = "badge-dot"), "Replicate aware"),
        span(class = "badge-pill", span(class = "badge-dot red"), "Stats ready"),
        span(class = "badge-pill", span(class = "badge-dot blue"), "Export polished")
      )
    ),
    div(
      class = "petri-stage",
      div(
        class = "petri-dish",
        div(class = "petri-ring"),
        span(class = "colony", style = "--x: 28%; --y: 24%; --s: 13px; --c: #6aa6a1; --d: 0s;"),
        span(class = "colony", style = "--x: 56%; --y: 18%; --s: 9px; --c: #d66b63; --d: .3s;"),
        span(class = "colony", style = "--x: 69%; --y: 43%; --s: 15px; --c: #4c78a8; --d: .6s;"),
        span(class = "colony", style = "--x: 34%; --y: 58%; --s: 10px; --c: #e5a84b; --d: .9s;"),
        span(class = "colony", style = "--x: 51%; --y: 68%; --s: 12px; --c: #6aa6a1; --d: 1.2s;"),
        span(class = "colony", style = "--x: 21%; --y: 43%; --s: 8px; --c: #d66b63; --d: 1.5s;"),
        span(class = "colony", style = "--x: 61%; --y: 57%; --s: 7px; --c: #31535a; --d: 1.8s;")
      )
    )
  ),
  div(
    class = "bench-strip",
    div(
      class = "bench-card",
      div(class = "bench-art", div(class = "pipette")),
      span(class = "bench-title", "Import replicate rows"),
      p(class = "bench-text", "Map your sample, treatment, time, replicate, and CFU columns without reshaping the file first.")
    ),
    div(
      class = "bench-card",
      div(
        class = "bench-art",
        div(
          class = "mini-plate",
          span(style = "--x: 18%; --y: 22%; --c: #6aa6a1;"),
          span(style = "--x: 58%; --y: 18%; --c: #d66b63;"),
          span(style = "--x: 42%; --y: 50%; --c: #31535a;"),
          span(style = "--x: 67%; --y: 62%; --c: #4c78a8;")
        )
      ),
      span(class = "bench-title", "Check the experiment"),
      p(class = "bench-text", "QC cards flag replicate structure before you trust error bars, comparisons, or exported tables.")
    ),
    div(
      class = "bench-card",
      div(
        class = "bench-art",
        div(
          class = "mini-bars",
          span(style = "--h: 22px; --c: #7aa6c2;"),
          span(style = "--h: 42px; --c: #4c78a8;"),
          span(style = "--h: 31px; --c: #d66b63;"),
          span(style = "--h: 54px; --c: #6aa6a1;")
        )
      ),
      span(class = "bench-title", "Export editable figures"),
      p(class = "bench-text", "Download SVG, PDF, high-DPI PNG, or PowerPoint with editable vector art when rvg is installed.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CFU CSV", accept = ".csv"),
      actionButton("use_demo", "Load dummy example data"),
      downloadButton("download_template", "Download dummy/template CSV"),
      tags$hr(),
      uiOutput("mapping_ui"),
      tags$hr(),
      selectInput(
        "plot_mode", "Plot mode",
        choices = c(
          "Combined samples, faceted by time" = "combined",
          "One sample, both timepoints" = "sample_both",
          "One sample, one timepoint" = "sample_time"
        )
      ),
      uiOutput("filter_ui"),
      selectInput("comparison", "Statistics shown on plot", choices = c(
        "Auto for selected plot" = "auto",
        "Sample/vector comparison" = "sample",
        "0 min vs 120 min" = "time",
        "Each treatment vs control" = "concentration_vs_control",
        "All treatment pairs" = "concentration_all",
        "None" = "none"
      )),
      uiOutput("control_ui"),
      selectInput("stats_method", "Statistical test", choices = c(
        "Welch t-test on log10(CFU)" = "welch",
        "Student t-test on log10(CFU)" = "student",
        "Linear model + emmeans" = "emmeans"
      ), selected = "welch"),
      selectInput("p_adjust", "Multiple-comparison correction", choices = c("BH", "holm", "bonferroni", "none"), selected = "BH"),
      selectInput("p_adjust_scope", "Correction scope", choices = c(
        "Across current table" = "global",
        "Within each panel/group" = "within_panel"
      ), selected = "global"),
      radioButtons("label_kind", "Statistic label", choices = c("Stars" = "stars", "Exact q values" = "q"), selected = "stars", inline = TRUE),
      checkboxInput("show_ns", "Show ns labels", value = FALSE),
      helpText("Default stats use replicate-level Welch t-tests on log10(CFU). Use emmeans when you want model-based marginal means."),
      tags$hr(),
      selectInput("y_mode", "Y-axis", choices = c("log10(CFU)" = "log10", "Raw CFU on log axis" = "raw_log_axis"), selected = "log10"),
      selectInput("error_type", "Variation summary", choices = c("SD", "SEM", "95% CI", "IQR", "Range (min-max)"), selected = "SD"),
      selectInput("variation_display", "Variation display", choices = c(
        "Capped error bars" = "errorbar",
        "Uncapped whiskers" = "linerange",
        "Mean point + whiskers" = "pointrange",
        "Mean crossbar interval" = "crossbar",
        "Replicate points only" = "none"
      ), selected = "errorbar"),
      helpText("IQR and min-max range show replicate spread directly. SD/SEM/95% CI summarize uncertainty around the mean."),
      checkboxInput("show_points", "Show replicate points", value = TRUE),
      uiOutput("axis_limit_ui"),
      actionButton("auto_y_axis", "Auto y-axis"),
      tags$hr(),
      actionButton("preset_publication", "Publication preset"),
      actionButton("preset_clean", "Clean no-stats preset"),
      tags$hr(),
      h4("Figure size"),
      fluidRow(
        column(4, actionButton("size_single_col", "Single column")),
        column(4, actionButton("size_double_col", "Double column")),
        column(4, actionButton("size_square", "Square"))
      ),
      helpText("These presets set export dimensions in inches so figures are reproducible across projects."),
      tags$hr(),
      textInput("plot_title", "Plot title", value = "CFU assay summary"),
      textInput("plot_subtitle", "Plot subtitle", value = "Stars show BH-adjusted comparisons: ns, * q<0.05, ** q<0.01, *** q<0.001"),
      checkboxInput("show_subtitle", "Show subtitle", value = TRUE),
      checkboxInput("hide_subtitle_no_stats", "Hide subtitle when statistics are set to None", value = TRUE),
      checkboxInput("show_method_caption", "Show methods caption (error-bar type, test, correction)", value = TRUE),
      textInput("x_label", "X-axis label", value = "Treatment"),
      textInput("treatment_unit", "Treatment unit suffix", value = ""),
      checkboxInput("append_treatment_unit", "Append unit to numeric treatment labels", value = FALSE),
      textInput("time_unit", "Time unit suffix", value = "min"),
      checkboxInput("append_time_unit", "Append unit to numeric time labels", value = TRUE),
      textInput("legend_title", "Legend title", value = ""),
      radioButtons("bar_orientation", "Bar orientation", choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"), selected = "vertical", inline = TRUE),
      selectInput("plot_theme", "Plot theme", choices = c(
        "Classic journal axes" = "classic",
        "Boxed panel" = "boxed",
        "Minimal grid" = "minimal_grid"
      ), selected = "classic"),
      checkboxInput("plot_box", "Enclose plot area in a box", value = FALSE),
      checkboxInput("show_y_ticks", "Show y-axis tick marks", value = TRUE),
      checkboxInput("show_minor_y_ticks", "Show minor y-axis tick marks", value = FALSE),
      checkboxInput("show_y_grid", "Show horizontal y guide lines", value = FALSE),
      checkboxInput("show_minor_y_grid", "Show minor horizontal guide lines", value = FALSE),
      numericInput("y_major_step", "Major y tick spacing", value = NA, min = 0),
      numericInput("y_minor_step", "Minor y tick spacing", value = NA, min = 0),
      helpText("Leave tick spacing blank for stable automatic ggplot ticks. Enter values only when you want fixed tick intervals."),
      sliderInput("y_tick_length", "Y tick length", min = 1, max = 10, value = 4),
      sliderInput("minor_y_tick_length", "Minor y tick length", min = 1, max = 8, value = 2),
      sliderInput("axis_line_width", "Axis line width", min = 0.2, max = 2, value = 0.6),
      sliderInput("box_line_width", "Box line width", min = 0.2, max = 2, value = 0.6),
      colourInput("axis_color", "Axis/stat line color", value = "#262626"),
      colourInput("grid_color", "Guide line color", value = "#DDDDDD"),
      colourInput("bar_outline_color", "Bar/point outline color", value = "#262626"),
      sliderInput("bar_outline_width", "Bar outline width", min = 0, max = 1.5, value = 0.25),
      sliderInput("errorbar_width", "Error bar line width", min = 0.2, max = 2, value = 0.55),
      actionButton("apply_okabe_ito", "Apply Okabe-Ito colorblind palette"),
      helpText("Okabe-Ito is a colorblind-safe qualitative palette (Wong 2011). Applies to sample, time, and single-bar colors."),
      colourInput("sample_color_1", "Sample color 1", value = "#0072B2"),
      colourInput("sample_color_2", "Sample color 2", value = "#D55E00"),
      colourInput("time_color_1", "0 min color", value = "#7AA6C2"),
      colourInput("time_color_2", "120 min color", value = "#2F5D8C"),
      colourInput("single_color", "Single-bar color", value = "#7AA6C2"),
      colourInput("stat_color", "Statistic label color", value = "#262626"),
      sliderInput("bar_width", "Bar width", min = 0.35, max = 0.95, value = 0.68),
      sliderInput("dodge_width", "Dodge width", min = 0.45, max = 1.1, value = 0.78),
      sliderInput("point_size", "Point size", min = 0.8, max = 4, value = 1.8),
      sliderInput("point_alpha", "Point alpha", min = 0.2, max = 1, value = 0.9),
      sliderInput("jitter_width", "Point jitter", min = 0, max = 0.25, value = 0.08),
      sliderInput("font_size", "Base font size", min = 8, max = 18, value = 11),
      sliderInput("title_size", "Title font size", min = 9, max = 24, value = 14),
      sliderInput("subtitle_size", "Subtitle font size", min = 7, max = 16, value = 10),
      sliderInput("stat_size", "Statistic label size", min = 2, max = 7, value = 3),
      sliderInput("x_angle", "X-label angle", min = 0, max = 70, value = 35),
      selectInput("legend_position", "Legend position", choices = c("top", "right", "bottom", "left", "inside", "none"), selected = "top"),
      sliderInput("legend_x", "Inside legend x", min = 0, max = 1, value = 0.98),
      sliderInput("legend_y", "Inside legend y", min = 0, max = 1, value = 0.98),
      sliderInput("legend_just_x", "Inside legend anchor x", min = 0, max = 1, value = 1),
      sliderInput("legend_just_y", "Inside legend anchor y", min = 0, max = 1, value = 1),
      tags$hr(),
      h4("Animation"),
      sliderInput("animation_fps", "GIF frames per second", min = 1, max = 20, value = 6),
      sliderInput("animation_duration", "GIF duration (seconds)", min = 1, max = 12, value = 4),
      numericInput("animation_dpi", "GIF resolution (DPI)", value = 150, min = 72, max = 300),
      checkboxInput("ppt_editable", "PowerPoint figure is editable vector art", value = TRUE),
      tags$hr(),
      h4("Reproducibility"),
      fileInput("preset_file", "Load plot preset JSON", accept = ".json"),
      downloadButton("download_preset", "Save plot preset"),
      downloadButton("download_manifest", "Analysis manifest"),
      downloadButton("download_r_script", "Export R script")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          br(),
          uiOutput("data_status"),
          div(
            class = "figure-guide",
            div(
              class = "guide-card",
              span(class = "guide-label", "Publication"),
              span(class = "guide-text", "Set exact inches, DPI, axis limits, tick spacing, fonts, and legend position before export.")
            ),
            div(
              class = "guide-card",
              span(class = "guide-label", "Editable"),
              span(class = "guide-text", "Use SVG/PDF for vector editing or PowerPoint with editable vector art when rvg is installed.")
            ),
            div(
              class = "guide-card",
              span(class = "guide-label", "Reusable"),
              span(class = "guide-text", "Size presets keep figure geometry consistent across CFU projects, talks, and manuscript panels.")
            )
          ),
          plotOutput("cfu_plot", height = "650px"),
          fluidRow(
            column(2, numericInput("download_width", "Width", value = 8.2, min = 3, max = 20)),
            column(2, numericInput("download_height", "Height", value = 4.8, min = 3, max = 20)),
            column(2, numericInput("download_dpi", "DPI", value = 600, min = 72, max = 1200)),
            column(2, downloadButton("download_png", "PNG")),
            column(2, downloadButton("download_pdf", "PDF")),
            column(2, downloadButton("download_svg", "SVG"))
          ),
          br(),
          fluidRow(
            column(3, downloadButton("download_gif", "Animated GIF")),
            column(3, downloadButton("download_pptx", "PowerPoint")),
            column(4, downloadButton("download_reveal_pptx", "Reveal slides PPT"))
          ),
          br(),
          actionButton("copy_plot", "Copy current plot"),
          span(id = "copy_plot_status", class = "copy-status")
        ),
        tabPanel("Cleaned data", br(), DTOutput("cleaned_table"), downloadButton("download_cleaned", "Download cleaned CSV")),
        tabPanel("Summary", br(), DTOutput("summary_table"), downloadButton("download_summary", "Download summary CSV")),
        tabPanel("Figure QA", br(), DTOutput("figure_qa_table"), downloadButton("download_figure_qa", "Download figure QA CSV")),
        tabPanel(
          "QC",
          br(),
          h4("Source checks"),
          DTOutput("raw_qc_table"),
          h4("Replicate groups"),
          DTOutput("replicate_qc_table"),
          downloadButton("download_qc", "Download QC CSV")
        ),
        tabPanel("Statistics", br(), DTOutput("stats_table"), downloadButton("download_stats", "Download current stats CSV")),
        tabPanel("ANOVA", br(), DTOutput("anova_table"), downloadButton("download_anova", "Download ANOVA CSV"))
      )
    )
  )
)

server <- function(input, output, session) {
  active_source <- reactiveVal("demo")

  observeEvent(input$use_demo, {
    active_source("demo")
  })

  observeEvent(input$file, {
    active_source("upload")
  })

  raw_data <- reactive({
    if (identical(active_source(), "upload") && !is.null(input$file)) {
      raw <- read_csv(input$file$datapath, show_col_types = FALSE, trim_ws = TRUE)
    } else {
      validate(need(file.exists(demo_file), "Upload a CSV or restore the dummy example data file."))
      raw <- read_csv(demo_file, show_col_types = FALSE, trim_ws = TRUE)
    }
    names(raw) <- clean_names(names(raw))
    raw
  })

  data_source_label <- reactive({
    if (identical(active_source(), "upload") && !is.null(input$file)) {
      paste0("Using uploaded file: ", input$file$name, ".")
    } else {
      "Using synthetic dummy example data."
    }
  })

  column_mapping <- reactive({
    req(input$col_sample, input$col_conc, input$col_time, input$col_rep, input$col_cfu)
    list(
      sample = input$col_sample,
      concentration = input$col_conc,
      time = input$col_time,
      replicate = input$col_rep,
      cfu = input$col_cfu
    )
  })

  output$axis_limit_ui <- renderUI({
    if (input$y_mode == "raw_log_axis") {
      tagList(
        numericInput("y_min", "Y minimum (raw CFU)", value = NA),
        numericInput("y_max", "Y maximum (raw CFU)", value = NA),
        helpText("For raw CFU on a log axis, enter raw CFU values such as 1e3 or 1e9. Major/minor tick spacing is entered in log10 intervals.")
      )
    } else {
      tagList(
        numericInput("y_min", "Y minimum (log10 CFU)", value = 0),
        numericInput("y_max", "Y maximum (log10 CFU)", value = NA),
        helpText("For log10(CFU), enter log10 values such as 0, 8, 10, or 12. Major/minor tick spacing uses the same log10 units.")
      )
    }
  })

  observeEvent(input$auto_y_axis, {
    if (identical(input$y_mode, "log10")) {
      updateNumericInput(session, "y_min", value = 0)
      updateNumericInput(session, "y_max", value = NA)
    } else {
      updateNumericInput(session, "y_min", value = NA)
      updateNumericInput(session, "y_max", value = NA)
    }
  })

  observeEvent(input$preset_publication, {
    updateSelectInput(session, "comparison", selected = "auto")
    updateRadioButtons(session, "label_kind", selected = "stars")
    updateCheckboxInput(session, "show_ns", value = FALSE)
    updateCheckboxInput(session, "show_points", value = TRUE)
    updateSelectInput(session, "error_type", selected = "SD")
    updateSelectInput(session, "variation_display", selected = "errorbar")
    updateCheckboxInput(session, "show_subtitle", value = TRUE)
    updateSelectInput(session, "plot_theme", selected = "classic")
    updateCheckboxInput(session, "plot_box", value = FALSE)
    updateCheckboxInput(session, "show_y_ticks", value = TRUE)
    updateCheckboxInput(session, "show_minor_y_ticks", value = FALSE)
    updateCheckboxInput(session, "show_y_grid", value = FALSE)
    updateCheckboxInput(session, "show_minor_y_grid", value = FALSE)
    updateSliderInput(session, "font_size", value = 11)
    updateSliderInput(session, "title_size", value = 14)
    updateSliderInput(session, "stat_size", value = 3)
    updateSliderInput(session, "bar_outline_width", value = 0.25)
    updateSliderInput(session, "errorbar_width", value = 0.55)
    updateSelectInput(session, "legend_position", selected = "top")
    updateTextInput(session, "plot_subtitle", value = "Stars show BH-adjusted comparisons: * q<0.05, ** q<0.01, *** q<0.001")
  })

  observeEvent(input$preset_clean, {
    updateSelectInput(session, "comparison", selected = "none")
    updateCheckboxInput(session, "show_subtitle", value = FALSE)
    updateCheckboxInput(session, "show_ns", value = FALSE)
    updateSelectInput(session, "variation_display", selected = "errorbar")
    updateSelectInput(session, "plot_theme", selected = "boxed")
    updateCheckboxInput(session, "plot_box", value = TRUE)
    updateCheckboxInput(session, "show_y_ticks", value = TRUE)
    updateCheckboxInput(session, "show_y_grid", value = TRUE)
    updateSliderInput(session, "font_size", value = 12)
    updateSliderInput(session, "title_size", value = 15)
  })

  observeEvent(input$apply_okabe_ito, {
    updateColourInput(session, "sample_color_1", value = okabe_ito[1])
    updateColourInput(session, "sample_color_2", value = okabe_ito[2])
    updateColourInput(session, "time_color_1", value = okabe_ito[1])
    updateColourInput(session, "time_color_2", value = okabe_ito[2])
    updateColourInput(session, "single_color", value = okabe_ito[1])
    showNotification("Applied Okabe-Ito colorblind-safe palette.", type = "message")
  })

  observeEvent(input$size_single_col, {
    updateNumericInput(session, "download_width", value = 3.35)
    updateNumericInput(session, "download_height", value = 2.65)
    updateNumericInput(session, "download_dpi", value = 600)
  })

  observeEvent(input$size_double_col, {
    updateNumericInput(session, "download_width", value = 7.0)
    updateNumericInput(session, "download_height", value = 4.2)
    updateNumericInput(session, "download_dpi", value = 600)
  })

  observeEvent(input$size_square, {
    updateNumericInput(session, "download_width", value = 4.5)
    updateNumericInput(session, "download_height", value = 4.5)
    updateNumericInput(session, "download_dpi", value = 600)
  })

  observeEvent(input$preset_file, {
    req(input$preset_file)
    validate(need(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required to load plot presets."))
    preset <- tryCatch(
      jsonlite::read_json(input$preset_file$datapath, simplifyVector = TRUE),
      error = function(e) {
        showNotification(paste("Preset could not be read:", conditionMessage(e)), type = "error")
        NULL
      }
    )
    if (is.null(preset)) return()
    settings <- preset$settings %||% preset
    for (id in intersect(names(settings), plot_setting_ids)) {
      value <- settings[[id]]
      if (id %in% select_setting_ids) {
        updateSelectInput(session, id, selected = value)
      } else if (id %in% radio_setting_ids) {
        updateRadioButtons(session, id, selected = value)
      } else if (id %in% checkbox_setting_ids) {
        updateCheckboxInput(session, id, value = isTRUE(value))
      } else if (id %in% text_setting_ids) {
        updateTextInput(session, id, value = as.character(value %||% ""))
      } else if (id %in% numeric_setting_ids) {
        updateNumericInput(session, id, value = suppressWarnings(as.numeric(value)))
      } else if (id %in% slider_setting_ids) {
        updateSliderInput(session, id, value = suppressWarnings(as.numeric(value)))
      }
    }
    showNotification("Plot preset loaded.", type = "message")
  })

  output$mapping_ui <- renderUI({
    cols <- names(raw_data())
    tagList(
      selectInput("col_sample", "Sample/vector column", choices = cols, selected = guess_column(cols, c("Sample", "sample", "strain", "vector"))),
      selectInput("col_conc", "Treatment/dose/condition column", choices = cols, selected = guess_column(cols, c("Treatment", "condition", "dose", "concentration"))),
      selectInput("col_time", "Time column", choices = cols, selected = guess_column(cols, c("Time", "time", "timepoint"))),
      selectInput("col_rep", "Replicate column", choices = cols, selected = guess_column(cols, c("Replicate", "replicate", "rep"))),
      selectInput("col_cfu", "CFU column", choices = cols, selected = guess_column(cols, c("CFU", "cfu", "count")))
    )
  })

  cfu_data <- reactive({
    prep_cfu_data(
      raw_data(),
      column_mapping(),
      treatment_unit = input$treatment_unit %||% "",
      time_unit = input$time_unit %||% "min",
      append_treatment_unit = isTRUE(input$append_treatment_unit),
      append_time_unit = isTRUE(input$append_time_unit)
    )
  })

  output$data_status <- renderUI({
    dat <- filtered_data()
    rep_counts <- dat %>%
      group_by(sample, concentration_label, time_min) %>%
      summarise(n_reps = n_distinct(replicate), .groups = "drop")
    min_reps <- if (nrow(rep_counts) > 0) min(rep_counts$n_reps, na.rm = TRUE) else NA_integer_
    max_reps <- if (nrow(rep_counts) > 0) max(rep_counts$n_reps, na.rm = TRUE) else NA_integer_
    source_is_upload <- identical(active_source(), "upload") && !is.null(input$file)
    source_text <- if (source_is_upload) {
      paste0("Uploaded: ", input$file$name)
    } else {
      "Synthetic dummy data"
    }
    rep_label <- if (is.na(min_reps)) {
      "No groups"
    } else if (identical(min_reps, max_reps)) {
      as.character(min_reps)
    } else {
      paste0(min_reps, "-", max_reps)
    }
    tip <- if (is.na(min_reps)) {
      div(class = "lab-tip warn", "No visible replicate groups yet. Choose samples and timepoints to build a plot.")
    } else if (min_reps < 2) {
      div(class = "lab-tip warn", "Some visible groups have fewer than 2 replicates, so SD/error bars and tests may be limited.")
    } else {
      div(class = "lab-tip ok", "Visible groups have replicate data for SD/error bars and replicate-level statistics.")
    }
    div(
      class = "lab-overview",
      div(
        class = "overview-top",
        div(class = "overview-title", "Visible experiment snapshot"),
        span(class = "source-chip", source_text)
      ),
      div(
        class = "metric-row",
        div(
          class = "metric-card",
          span(class = "metric-value", format(nrow(dat), big.mark = ",")),
          span(class = "metric-label", "Rows"),
          span(class = "metric-note", "After current filters")
        ),
        div(
          class = "metric-card",
          span(class = "metric-value", n_distinct(dat$sample)),
          span(class = "metric-label", "Samples"),
          span(class = "metric-note", "Vectors or individuals")
        ),
        div(
          class = "metric-card",
          span(class = "metric-value", n_distinct(dat$concentration_label)),
          span(class = "metric-label", "Treatments"),
          span(class = "metric-note", "Dose or condition groups")
        ),
        div(
          class = "metric-card",
          span(class = "metric-value", rep_label),
          span(class = "metric-label", "Replicates"),
          span(class = "metric-note", "Range per visible group")
        )
      ),
      tip,
      tags$span(
        style = "display:none;",
        data_source_label()
      )
    )
  })

  output$filter_ui <- renderUI({
    dat <- cfu_data()
    if (input$plot_mode == "combined") {
      tagList(
        selectizeInput("samples", "Samples to include", choices = levels(dat$sample), selected = levels(dat$sample), multiple = TRUE),
        selectizeInput("times", "Timepoints to include", choices = levels(dat$time_min), selected = levels(dat$time_min), multiple = TRUE)
      )
    } else if (input$plot_mode == "sample_both") {
      tagList(
        selectInput("single_sample", "Sample/vector", choices = levels(dat$sample), selected = levels(dat$sample)[1]),
        selectizeInput("times", "Timepoints to include", choices = levels(dat$time_min), selected = levels(dat$time_min), multiple = TRUE)
      )
    } else {
      tagList(
        selectInput("single_sample", "Sample/vector", choices = levels(dat$sample), selected = levels(dat$sample)[1]),
        selectInput("single_time", "Timepoint", choices = levels(dat$time_min), selected = levels(dat$time_min)[1])
      )
    }
  })

  output$control_ui <- renderUI({
    dat <- cfu_data()
    selectInput("control_concentration", "Control treatment", choices = levels(dat$concentration_label), selected = levels(dat$concentration_label)[1])
  })

  filtered_data <- reactive({
    dat <- cfu_data()
    if (input$plot_mode == "combined") {
      req(input$samples, input$times)
      dat %>% filter(sample %in% input$samples, time_min %in% input$times) %>% droplevels()
    } else if (input$plot_mode == "sample_both") {
      req(input$single_sample, input$times)
      dat %>% filter(sample == input$single_sample, time_min %in% input$times) %>% droplevels()
    } else {
      req(input$single_sample, input$single_time)
      dat %>% filter(sample == input$single_sample, time_min == input$single_time) %>% droplevels()
    }
  })

  active_comparison <- reactive({
    if (input$comparison != "auto") return(input$comparison)
    switch(
      input$plot_mode,
      combined = "sample",
      sample_both = "time",
      sample_time = "concentration_vs_control",
      "sample"
    )
  })

  current_summary <- reactive({
    plot_summary(filtered_data(), input$plot_mode, input$y_mode, input$error_type)
  })

  current_stats <- reactive({
    cmp <- active_comparison()
    if (cmp == "none") return(tibble())
    if (identical(input$stats_method, "emmeans")) {
      run_contrast(filtered_data(), cmp, input$p_adjust, input$control_concentration)
    } else {
      run_groupwise_t_tests(
        dat = filtered_data(),
        comparison = cmp,
        p_adjust = input$p_adjust,
        adjustment_scope = input$p_adjust_scope,
        control_concentration = input$control_concentration,
        ttest_type = input$stats_method
      )
    }
  })

  current_anova <- reactive({
    run_anova(filtered_data())
  })

  current_annotation <- reactive({
    annotation_data(current_stats(), current_summary(), active_comparison(), input$plot_mode, input$label_kind, input$show_ns, input$y_mode)
  })

  figure_qa <- reactive({
    dat <- filtered_data()
    sumdat <- current_summary()
    stats <- current_stats()
    rep_counts <- dat %>%
      group_by(sample, concentration_label, time_min) %>%
      summarise(n_reps = n_distinct(replicate), .groups = "drop")
    max_label_chars <- max(nchar(as.character(levels(droplevels(dat$concentration_label)))), na.rm = TRUE)
    qa <- tibble(
      check = character(),
      status = character(),
      details = character()
    )
    add_check <- function(check, pass, details_ok, details_warn) {
      tibble(
        check = check,
        status = if (isTRUE(pass)) "OK" else "Review",
        details = if (isTRUE(pass)) details_ok else details_warn
      )
    }
    bind_rows(
      qa,
      add_check(
        "Replicate groups",
        nrow(rep_counts) > 0 && min(rep_counts$n_reps, na.rm = TRUE) >= 2,
        "All visible groups have at least 2 replicates.",
        "At least one visible group has fewer than 2 replicates."
      ),
      add_check(
        "Replicate points",
        isTRUE(input$show_points),
        "Replicate points are shown.",
        "Replicate points are hidden; consider showing them for small-n CFU assays."
      ),
      add_check(
        "Export resolution",
        !is.na(input$download_dpi) && input$download_dpi >= 300,
        paste0("DPI is ", input$download_dpi, "."),
        paste0("DPI is ", input$download_dpi, "; use at least 300, preferably 600 for raster exports.")
      ),
      add_check(
        "Figure size",
        !is.na(input$download_width) && !is.na(input$download_height) && input$download_width >= 3 && input$download_height >= 2.5,
        paste0("Export size is ", input$download_width, " x ", input$download_height, " in."),
        "Export width/height may be too small for manuscript text and labels."
      ),
      add_check(
        "Base font size",
        !is.na(input$font_size) && input$font_size >= 9,
        paste0("Base font size is ", input$font_size, "."),
        "Base font size is below 9; check readability after export."
      ),
      add_check(
        "X-label density",
        max_label_chars <= 14 || input$x_angle >= 30 || identical(input$bar_orientation, "horizontal"),
        "Treatment labels should fit with the current angle/orientation.",
        "Long treatment labels may overlap; increase x-label angle or use horizontal bars."
      ),
      add_check(
        "Statistics labels",
        identical(active_comparison(), "none") || nrow(stats) == 0 || !is.na(axis_limit(input$y_max)) || identical(input$label_kind, "q"),
        "Statistics display is unlikely to clip.",
        "When star labels are shown, set a y-axis maximum if labels get clipped."
      ),
      add_check(
        "Legend placement",
        !identical(input$legend_position, "inside") || n_distinct(dat$sample) <= 2,
        "Legend placement should be manageable.",
        "Inside legend may cover data when many groups are shown; inspect before export."
      )
    )
  })

  current_plot <- reactive({
    validate(need(nrow(filtered_data()) > 0, "No rows remain after filtering."))
    make_cfu_plot(
      dat = filtered_data(),
      sumdat = current_summary(),
      ann = current_annotation(),
      plot_mode = input$plot_mode,
      y_mode = input$y_mode,
      error_type = input$error_type,
      input = input
    )
  })

  output$cfu_plot <- renderPlot({
    current_plot()
  }, res = 120)

  output$cleaned_table <- renderDT({
    datatable(cfu_data(), options = list(pageLength = 12, scrollX = TRUE))
  })

  output$summary_table <- renderDT({
    datatable(summary_cfu(filtered_data()), options = list(pageLength = 12, scrollX = TRUE))
  })

  output$raw_qc_table <- renderDT({
    datatable(raw_qc_summary(raw_data(), column_mapping()), options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$replicate_qc_table <- renderDT({
    datatable(qc_summary(cfu_data()), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$stats_table <- renderDT({
    datatable(current_stats(), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$anova_table <- renderDT({
    datatable(current_anova(), options = list(pageLength = 12, scrollX = TRUE))
  })

  output$figure_qa_table <- renderDT({
    datatable(figure_qa(), options = list(dom = "t", pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  save_plot_file <- function(file, device) {
    ggsave(
      filename = file,
      plot = current_plot(),
      width = input$download_width,
      height = input$download_height,
      dpi = input$download_dpi,
      device = device
    )
  }

  add_plot_slide <- function(doc, plot_obj) {
    doc <- officer::add_slide(doc, layout = "Blank", master = "Office Theme")
    if (isTRUE(input$ppt_editable) && requireNamespace("rvg", quietly = TRUE)) {
      officer::ph_with(doc, rvg::dml(ggobj = plot_obj), location = officer::ph_location_fullsize())
    } else {
      tmp <- tempfile(fileext = ".png")
      ggsave(
        filename = tmp,
        plot = plot_obj,
        width = input$download_width,
        height = input$download_height,
        dpi = input$animation_dpi
      )
      officer::ph_with(doc, officer::external_img(tmp), location = officer::ph_location_fullsize())
    }
  }

  save_pptx_file <- function(file) {
    validate(need(requireNamespace("officer", quietly = TRUE), "Package officer is required for PowerPoint export."))
    doc <- officer::read_pptx()
    doc <- add_plot_slide(doc, current_plot())
    print(doc, target = file)
  }

  save_reveal_pptx_file <- function(file) {
    validate(need(requireNamespace("officer", quietly = TRUE), "Package officer is required for PowerPoint export."))
    reveal <- reveal_data(filtered_data(), current_summary(), input$plot_mode)
    total_steps <- nrow(reveal$groups)
    validate(need(total_steps > 0, "No bars are available for reveal slides."))

    doc <- officer::read_pptx()
    for (step in seq_len(total_steps)) {
      step_plot <- make_reveal_plot(
        dat = filtered_data(),
        sumdat = current_summary(),
        plot_mode = input$plot_mode,
        y_mode = input$y_mode,
        error_type = input$error_type,
        input = input,
        step = step
      ) +
        labs(caption = paste0("Reveal step ", step, " of ", total_steps))
      doc <- add_plot_slide(doc, step_plot)
    }
    print(doc, target = file)
  }

  csv_literal <- function(x) {
    validate(need(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required for reproducibility exports."))
    txt <- paste(utils::capture.output(utils::write.csv(x, row.names = FALSE, na = "")), collapse = "\n")
    as.character(jsonlite::toJSON(txt, auto_unbox = TRUE))
  }

  dput_literal <- function(x) {
    paste(utils::capture.output(dput(x)), collapse = "\n")
  }

  manifest_payload <- reactive({
    list(
      app = "CFU Plot Studio",
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      data_source = data_source_label(),
      column_mapping = column_mapping(),
      plot_mode = input$plot_mode,
      active_comparison = active_comparison(),
      settings = collect_plot_settings(input),
      visible_data = list(
        rows = nrow(filtered_data()),
        samples = as.character(levels(droplevels(filtered_data()$sample))),
        treatments = as.character(levels(droplevels(filtered_data()$concentration_label))),
        timepoints = as.character(levels(droplevels(filtered_data()$time_min)))
      ),
      figure_qa = figure_qa(),
      packages = list(
        R = as.character(getRversion()),
        shiny = as.character(utils::packageVersion("shiny")),
        ggplot2 = as.character(utils::packageVersion("ggplot2")),
        dplyr = as.character(utils::packageVersion("dplyr")),
        readr = as.character(utils::packageVersion("readr")),
        emmeans = as.character(utils::packageVersion("emmeans"))
      )
    )
  })

  reproducible_script <- reactive({
    validate(need(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required for R script export."))
    settings <- collect_plot_settings(input)
    dat_csv <- csv_literal(filtered_data())
    sum_csv <- csv_literal(current_summary())
    ann_csv <- csv_literal(current_annotation())
    settings_code <- dput_literal(settings)
    plot_mode_code <- dput_literal(input$plot_mode)
    y_mode_code <- dput_literal(input$y_mode)

    paste(c(
      "# Reproducible CFU Plot Studio figure export",
      paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      "# This script rebuilds the visible figure from embedded filtered data, summary data, annotations, and plot settings.",
      "",
      "library(ggplot2)",
      "",
      paste0("dat <- read.csv(text = ", dat_csv, ", check.names = FALSE)"),
      paste0("sumdat <- read.csv(text = ", sum_csv, ", check.names = FALSE)"),
      paste0("ann <- read.csv(text = ", ann_csv, ", check.names = FALSE)"),
      "",
      "for (nm in intersect(c('sample', 'concentration_label', 'time_min'), names(dat))) dat[[nm]] <- factor(dat[[nm]], levels = unique(dat[[nm]]))",
      "for (nm in intersect(c('sample', 'concentration_label', 'time_min'), names(sumdat))) sumdat[[nm]] <- factor(sumdat[[nm]], levels = unique(sumdat[[nm]]))",
      "if ('concentration_label' %in% names(ann)) ann$concentration_label <- factor(ann$concentration_label, levels = levels(sumdat$concentration_label))",
      "",
      paste0("settings <- ", settings_code),
      paste0("plot_mode <- ", plot_mode_code),
      paste0("y_mode <- ", y_mode_code),
      "",
      "`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x",
      "named_palette <- function(levels_vec, seed_colors) {",
      "  levels_vec <- as.character(levels_vec)",
      "  seed_colors <- unname(seed_colors)",
      "  if (length(levels_vec) <= length(seed_colors)) setNames(seed_colors[seq_along(levels_vec)], levels_vec)",
      "  else setNames(c(seed_colors, grDevices::hcl.colors(length(levels_vec) - length(seed_colors), palette = 'Dark 3')), levels_vec)",
      "}",
      "",
      "axis_col <- settings$axis_color %||% 'grey15'",
      "outline_col <- settings$bar_outline_color %||% 'grey20'",
      "variation_display <- settings$variation_display %||% 'errorbar'",
      "dodge_pos <- position_dodge(width = settings$dodge_width %||% 0.78)",
      "base_theme <- theme_classic(base_size = settings$font_size %||% 11) +",
      "  theme(",
      "    plot.title = element_text(face = 'bold', size = settings$title_size %||% 14),",
      "    plot.subtitle = element_text(size = settings$subtitle_size %||% 10, color = 'grey30'),",
      "    axis.text.x = element_text(angle = settings$x_angle %||% 35, hjust = 1),",
      "    legend.position = settings$legend_position %||% 'top',",
      "    strip.background = element_rect(fill = 'grey92', color = NA),",
      "    strip.text = element_text(face = 'bold'),",
      "    axis.ticks.y = element_line(color = axis_col, linewidth = settings$axis_line_width %||% 0.6),",
      "    axis.line = element_line(color = axis_col, linewidth = settings$axis_line_width %||% 0.6),",
      "    panel.grid.major.y = if (isTRUE(settings$show_y_grid)) element_line(color = settings$grid_color %||% 'grey87', linewidth = 0.3) else element_blank(),",
      "    panel.border = if (isTRUE(settings$plot_box)) element_rect(color = axis_col, fill = NA, linewidth = settings$box_line_width %||% 0.6) else element_blank()",
      "  )",
      "",
      "add_interval <- function(p, pos, width = 0.22) {",
      "  if (identical(variation_display, 'none')) return(p)",
      "  if (identical(variation_display, 'linerange')) return(p + geom_linerange(aes(ymin = ymin, ymax = ymax), position = pos, linewidth = settings$errorbar_width %||% 0.55, color = axis_col, show.legend = FALSE))",
      "  if (identical(variation_display, 'pointrange')) return(p + geom_pointrange(aes(y = mean_y, ymin = ymin, ymax = ymax), position = pos, linewidth = settings$errorbar_width %||% 0.55, color = axis_col, show.legend = FALSE))",
      "  if (identical(variation_display, 'crossbar')) return(p + geom_crossbar(aes(y = mean_y, ymin = ymin, ymax = ymax), position = pos, width = width * 1.35, linewidth = settings$errorbar_width %||% 0.55, color = axis_col, fill = NA, show.legend = FALSE))",
      "  p + geom_errorbar(aes(ymin = ymin, ymax = ymax), position = pos, width = width, linewidth = settings$errorbar_width %||% 0.55, color = axis_col, show.legend = FALSE)",
      "}",
      "",
      "subtitle_text <- if (isTRUE(settings$show_subtitle)) settings$plot_subtitle else NULL",
      "y_lab <- if (identical(y_mode, 'log10')) expression(log[10]~'CFU') else 'CFU'",
      "",
      "# Methods caption: names error-bar type and (when stats shown) test + correction.",
      "caption_text <- NULL",
      "if (isTRUE(settings$show_method_caption %||% TRUE)) {",
      "  err_txt <- switch(settings$error_type %||% 'SD',",
      "    'SD' = 'Error bars show mean ± SD', 'SEM' = 'Error bars show mean ± SEM',",
      "    '95% CI' = 'Error bars show mean with 95% CI', 'IQR' = 'Intervals show median IQR (Q1-Q3)',",
      "    'Range (min-max)' = 'Intervals show min-max range', paste0('Error bars: ', settings$error_type))",
      "  if (identical(settings$variation_display %||% 'errorbar', 'none')) err_txt <- NULL",
      "  parts <- err_txt",
      "  if (!identical(settings$comparison %||% 'auto', 'none')) {",
      "    test_txt <- switch(settings$stats_method %||% 'welch', 'welch' = 'Welch t-test on log10(CFU)',",
      "      'student' = 'Student t-test on log10(CFU)', 'emmeans' = 'linear model + emmeans on log10(CFU)', 'statistical test')",
      "    corr_txt <- switch(settings$p_adjust %||% 'BH', 'BH' = 'Benjamini-Hochberg FDR', 'holm' = 'Holm',",
      "      'bonferroni' = 'Bonferroni', 'none' = 'no', settings$p_adjust)",
      "    parts <- c(parts, paste0(test_txt, '; ', corr_txt, ' correction'))",
      "  }",
      "  if (length(parts) > 0) caption_text <- paste0(paste(parts, collapse = '; '), '.')",
      "}",
      "",
      "if (identical(plot_mode, 'combined')) {",
      "  pal <- named_palette(levels(sumdat$sample), c(settings$sample_color_1, settings$sample_color_2))",
      "  p <- ggplot(sumdat, aes(concentration_label, mean_y, fill = sample)) +",
      "    geom_col(position = dodge_pos, width = settings$bar_width %||% 0.68, color = outline_col, linewidth = settings$bar_outline_width %||% 0.25)",
      "  p <- add_interval(p, dodge_pos)",
      "  if (isTRUE(settings$show_points)) p <- p + geom_point(data = dat, aes(concentration_label, if (identical(y_mode, 'log10')) log10_cfu else cfu, fill = sample), position = position_jitterdodge(jitter.width = settings$jitter_width %||% 0.08, dodge.width = settings$dodge_width %||% 0.78), shape = 21, size = settings$point_size %||% 1.8, color = outline_col, stroke = 0.25, alpha = settings$point_alpha %||% 0.9)",
      "  p <- p + facet_wrap(~ time_min, nrow = 1) + scale_fill_manual(values = pal)",
      "} else if (identical(plot_mode, 'sample_both')) {",
      "  pal <- named_palette(levels(sumdat$time_min), c(settings$time_color_1, settings$time_color_2))",
      "  p <- ggplot(sumdat, aes(concentration_label, mean_y, fill = time_min)) +",
      "    geom_col(position = dodge_pos, width = settings$bar_width %||% 0.68, color = outline_col, linewidth = settings$bar_outline_width %||% 0.25)",
      "  p <- add_interval(p, dodge_pos)",
      "  if (isTRUE(settings$show_points)) p <- p + geom_point(data = dat, aes(concentration_label, if (identical(y_mode, 'log10')) log10_cfu else cfu, fill = time_min), position = position_jitterdodge(jitter.width = settings$jitter_width %||% 0.08, dodge.width = settings$dodge_width %||% 0.78), shape = 21, size = settings$point_size %||% 1.8, color = outline_col, stroke = 0.25, alpha = settings$point_alpha %||% 0.9)",
      "  p <- p + scale_fill_manual(values = pal)",
      "} else {",
      "  p <- ggplot(sumdat, aes(concentration_label, mean_y)) +",
      "    geom_col(width = settings$bar_width %||% 0.68, fill = settings$single_color %||% '#7AA6C2', color = outline_col, linewidth = settings$bar_outline_width %||% 0.25)",
      "  p <- add_interval(p, position_identity(), width = 0.2)",
      "  if (isTRUE(settings$show_points)) p <- p + geom_point(data = dat, aes(concentration_label, if (identical(y_mode, 'log10')) log10_cfu else cfu), position = position_jitter(width = settings$jitter_width %||% 0.08, height = 0), shape = 21, size = settings$point_size %||% 1.8, fill = settings$single_color %||% '#7AA6C2', color = outline_col, stroke = 0.25, alpha = settings$point_alpha %||% 0.9)",
      "}",
      "",
      "p <- p + scale_x_discrete(drop = FALSE) + labs(x = settings$x_label %||% 'Treatment', y = y_lab, fill = settings$legend_title %||% '', title = settings$plot_title %||% '', subtitle = subtitle_text, caption = caption_text) + base_theme + theme(plot.caption = element_text(size = (settings$subtitle_size %||% 10) * 0.92, color = 'grey35', hjust = 0))",
      "if (nrow(ann) > 0 && all(c('x', 'y', 'label') %in% names(ann))) p <- p + geom_text(data = ann, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = settings$stat_size %||% 3, color = settings$stat_color %||% 'grey15')",
      "if (identical(y_mode, 'raw_log_axis')) p <- p + scale_y_log10() else p <- p + scale_y_continuous(expand = expansion(mult = c(0, 0.08)))",
      "if (identical(settings$bar_orientation, 'horizontal')) p <- p + coord_flip(clip = 'off')",
      "",
      "print(p)",
      "ggsave('cfu_plot_recreated.png', p, width = settings$download_width %||% 8.2, height = settings$download_height %||% 4.8, dpi = settings$download_dpi %||% 600)"
    ), collapse = "\n")
  })

  output$download_png <- downloadHandler(
    filename = function() "cfu_plot.png",
    content = function(file) save_plot_file(file, "png")
  )

  output$download_pdf <- downloadHandler(
    filename = function() "cfu_plot.pdf",
    content = function(file) save_plot_file(file, cairo_pdf)
  )

  output$download_svg <- downloadHandler(
    filename = function() "cfu_plot.svg",
    content = function(file) save_plot_file(file, "svg")
  )

  output$download_gif <- downloadHandler(
    filename = function() "cfu_bar_reveal.gif",
    content = function(file) {
      anim <- make_animated_cfu_plot(
        dat = filtered_data(),
        sumdat = current_summary(),
        plot_mode = input$plot_mode,
        y_mode = input$y_mode,
        error_type = input$error_type,
        input = input
      )
      nframes <- max(anim$steps, round(input$animation_duration * input$animation_fps))
      rendered <- gganimate::animate(
        anim$plot,
        nframes = nframes,
        fps = input$animation_fps,
        width = input$download_width,
        height = input$download_height,
        units = "in",
        res = input$animation_dpi,
        renderer = gganimate::gifski_renderer()
      )
      gganimate::anim_save(file, animation = rendered)
    }
  )

  output$download_pptx <- downloadHandler(
    filename = function() "cfu_plot.pptx",
    content = function(file) save_pptx_file(file)
  )

  output$download_reveal_pptx <- downloadHandler(
    filename = function() "cfu_bar_reveal_slides.pptx",
    content = function(file) save_reveal_pptx_file(file)
  )

  output$download_template <- downloadHandler(
    filename = function() "dummy_cfu_template.csv",
    content = function(file) file.copy(demo_file, file, overwrite = TRUE)
  )

  output$download_preset <- downloadHandler(
    filename = function() "cfu_plot_preset.json",
    content = function(file) {
      validate(need(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required for plot preset export."))
      writeLines(jsonlite::toJSON(plot_settings_payload(input), pretty = TRUE, auto_unbox = TRUE, null = "null"), file)
    }
  )

  output$download_manifest <- downloadHandler(
    filename = function() "cfu_analysis_manifest.json",
    content = function(file) {
      validate(need(requireNamespace("jsonlite", quietly = TRUE), "Package jsonlite is required for manifest export."))
      writeLines(jsonlite::toJSON(manifest_payload(), pretty = TRUE, auto_unbox = TRUE, null = "null"), file)
    }
  )

  output$download_r_script <- downloadHandler(
    filename = function() "recreate_cfu_plot.R",
    content = function(file) writeLines(reproducible_script(), file)
  )

  output$download_cleaned <- downloadHandler(
    filename = function() "cleaned_cfu_data.csv",
    content = function(file) write_csv(cfu_data(), file)
  )

  output$download_summary <- downloadHandler(
    filename = function() "summary_statistics.csv",
    content = function(file) write_csv(summary_cfu(filtered_data()), file)
  )

  output$download_qc <- downloadHandler(
    filename = function() "cfu_qc_summary.csv",
    content = function(file) {
      qc_out <- bind_rows(
        raw_qc_summary(raw_data(), column_mapping()) %>%
          mutate(section = "source", value = as.character(value), .before = 1),
        qc_summary(cfu_data()) %>%
          mutate(section = "replicate_groups", check = flag, value = as.character(replicates), .before = 1) %>%
          select(section, check, value, everything())
      )
      write_csv(qc_out, file)
    }
  )

  output$download_figure_qa <- downloadHandler(
    filename = function() "cfu_figure_qa.csv",
    content = function(file) write_csv(figure_qa(), file)
  )

  output$download_stats <- downloadHandler(
    filename = function() "cfu_statistics.csv",
    content = function(file) write_csv(current_stats(), file)
  )

  output$download_anova <- downloadHandler(
    filename = function() "cfu_anova.csv",
    content = function(file) write_csv(current_anova(), file)
  )
}

shinyApp(ui, server)
