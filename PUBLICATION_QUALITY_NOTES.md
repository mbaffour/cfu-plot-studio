# Publication-Quality Audit Notes

This document records a publication-readiness audit of CFU Plot Studio (`app.R`)
against a rubric covering colorblind-safe color, labeled error bars, reported n,
named statistical test + correction, and publication-grade export settings.

**IMPORTANT:** All code changes below are **UNVERIFIED**. There was no R runtime
available during this review, so the app was **not run**. Please launch the app
and smoke-test each changed control before merging.

The app already had a strong baseline: emmeans/broom statistics, PNG/PDF/SVG/GIF/PPTX
export, size/DPI presets, a reproducible R-script export, a Figure QA tab that already
checks DPI (>=300), figure size, and font size, and a QC tab reporting replicates and n.
The changes below fill the remaining gaps that were safe to implement without altering
statistical methodology.

## (a) Changes made

1. **Okabe-Ito colorblind-safe palette (option + safer defaults).**
   - Added an `okabe_ito` color constant (Wong 2011, *Nat Methods* 8:441).
   - Added an **"Apply Okabe-Ito colorblind palette"** button in the sidebar
     (`actionButton("apply_okabe_ito", ...)`) with an `observeEvent` that updates
     the sample, time, and single-bar color inputs via `updateColourInput`.
   - Changed the two default sample colors to Okabe-Ito blue (`#0072B2`) and
     vermillion (`#D55E00`) so the default two-group figure is colorblind-safe
     out of the box. (Time colors and single-bar color left as their existing
     sequential-blue defaults to keep the diff surgical; the button converts them.)

2. **Auto "methods caption" naming the error-bar type, test, and correction.**
   - Added helpers `error_type_caption()` and `stats_caption()`.
   - `make_cfu_plot()` now builds a caption such as:
     *"Error bars show mean ± SD; Welch t-test on log10(CFU); Benjamini-Hochberg
     FDR correction."* It names the SD/SEM/95% CI/IQR/range interval and, when
     statistics are shown, the test + multiple-comparison correction. This makes
     the figure self-documenting, addressing "error bars labeled" and
     "significance annotation names the test + correction."
   - Added checkbox **"Show methods caption ..."** (`show_method_caption`,
     default ON) so users can suppress it. Registered in the preset setting IDs
     so it round-trips through preset save/load.
   - The reproducible R-script export was updated to emit the same caption logic,
     so exported scripts reproduce the labeled figure (accuracy of reproducible
     methods text).

### Why these are low-risk
- The palette button follows the exact pattern of the existing preset buttons and
  only calls `updateColourInput` (from the already-loaded `colourpicker` pkg).
- The caption is purely additive (`labs(caption=)` + a `plot.caption` theme line).
  The reveal/animation code sets its own `labs(caption=)` *after* calling
  `make_cfu_plot`, so it overrides the methods caption with no conflict.
- All new inputs are read with `%||%` fallbacks, so a missing input cannot error.

## (b) Deferred recommendations (need the author's scientific decision or an R test-run)

These were intentionally **not** applied because they change methodology, add
non-trivial layout logic, or need a live R run to validate:

1. **Display per-group n on the figure.** n is currently in the Summary/QC tabs and
   the snapshot cards, but not drawn on the plot. Adding "n=" labels under each bar
   (e.g. via a `geom_text` layer keyed to `sumdat$n`) is standard for publication
   but requires layout/positioning work and testing against every plot mode,
   orientation, and log axis — defer until it can be run.

2. **Effect sizes / confidence intervals in the stats output.** The t-test path
   reports the log10 difference and fold-change but not a CI on that difference or a
   standardized effect size (Cohen's d / Hedges' g). Adding the t-test CI
   (`conf.int` from `t.test`) and an effect-size column would strengthen the
   analysis. Additive but should be verified with R.

3. **Normality / variance diagnostics.** log10(CFU) with a t-test assumes
   approximate normality of residuals; for small n a non-parametric option
   (Wilcoxon/Mann-Whitney) or an explicit normality note may be warranted. This is
   a **methodology choice** — flagged, not changed.

4. **ANOVA assumption reporting.** `run_anova` fits an `lm` and reports the ANOVA
   table but no residual diagnostics or type (Type I sequential SS via `anova()`).
   For unbalanced designs, Type II/III SS (e.g. `car::Anova`) may be more
   appropriate. **Methodology choice** — flagged, not changed.

5. **Default multiple-comparison correction vs. the preset subtitle.** The default
   `plot_subtitle` and the "Publication preset" subtitle hardcode "BH-adjusted",
   which can be inaccurate if the user selects Holm/Bonferroni/none. The new dynamic
   methods caption reports the actual correction, so this is mitigated, but you may
   want to drop the hardcoded wording from the subtitle presets.

6. **PDF/PPTX raster fallback DPI.** The PNG fallback inside `add_plot_slide` uses
   `input$animation_dpi` (default 150) rather than `input$download_dpi` (default
   600). For PowerPoint export when `rvg` is unavailable, consider using
   `download_dpi`. Left unchanged to avoid altering export behavior without a test.

7. **Physical size correctness.** Export dimensions are in inches with configurable
   DPI (good). The GIF path passes `units = "in"`; the PNG/PDF/SVG `ggsave` calls
   rely on ggsave's default `units = "in"` — correct, but worth confirming on a run.
