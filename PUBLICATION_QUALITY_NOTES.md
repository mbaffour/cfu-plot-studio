# Publication-Quality Audit Notes

## Round 2 — 2026-08-24 (verified against a live R 4.5.0 / ggplot2 4.0.2 run)

Unlike round 1, everything below was **executed**: the app was launched, driven in a
browser, and every change was checked against real data
(`gp75 cfu all reps_vault dummy.csv`, a two-construct inducer dose-response with
2 timepoints and 3 replicates).

### Bug fixed: column auto-detection deleted capital letters

`guess_column()` ran `gsub("[^a-z0-9]+", "", cols)` *before* `tolower()`. The character
class only permits lowercase, so every capital in a header was **deleted rather than
folded**: `"Sample"` normalised to `"ample"`, `"Time"` to `"ime"`, `"CFU"` to `""`.

It appeared to work only because the candidate lists happened to contain capitalised
variants that were mangled identically (`"Sample"` also became `"ample"`, so it matched
itself). The failures were real:

- On the bundled `dummy_cfu_example.csv`, the **Treatment dropdown selected `Group`**, not `Treatment`.
- On the gp75 file, the Treatment dropdown selected `Sample`, because `inducer_concentration` is not an exact match for `concentration`.
- Any file with a leading all-caps column hijacked **both** the treatment and CFU dropdowns, because `"CFU"` normalised to the empty string and the empty string matches the first all-caps header.

Fixed by lowercasing first and adding a substring pass (so `inducer_concentration` finds
`concentration` and `CFU_per_mL` finds `cfu`), plus wider bench synonym lists. Six
header-shape cases are covered by `tests/test_column_matching.R`.

### Preview now matches the export geometry

`plotOutput("cfu_plot", height = "650px")` gave a fixed-height, container-width preview
regardless of the export size. Choosing the single-column preset (1.26:1) still previewed
at roughly 1.7:1, so fonts and spacing were tuned against a figure shape that was never
exported.

The preview is now rendered at `export_inches x 144` pixels with the device resolution
also set to 144, so relative text size is exact. Verified live: 8.2 x 4.8 in gave a
1181 x 691 px canvas (ratio 1.709); the Nature single-column preset gave 504 x 397
(ratio 1.270).

`res` is fixed rather than user-adjustable on purpose — Shiny forces `renderPlot`'s `res`
promise once and never re-reads it, so a reactive zoom would silently stop working.

### Figure size in inches or millimetres

Journals specify widths in mm. Width/height are entered in either unit, and switching
converts the numbers so the physical size is preserved (verified: 3.5 in became 88.9 mm
with the readout unchanged at 3.50 x 2.76 in). Every export path converts to inches at a
single boundary, `size_to_inches()`, and `ggsave` calls now pass `units = "in"`
explicitly instead of relying on the default.

Added Nature single-column (89 mm) and double-column (183 mm) presets.

### Bars versus points (deferred item, now implemented)

Bar length is only meaningful measured from zero, and on a `log10(CFU)` axis zero means
1 CFU/mL. For counts spanning 10^8-10^11 that wastes most of the panel and encodes
nothing. **Mean shown as** now offers points, which lets the axis be framed on the data.

*Auto y-axis* is aware of the distinction: it pins the zero baseline for bars and fits
the range to the data for points. Cropping a **bar** axis away from zero misstates every
ratio a reader takes off it, so that path is deliberately not offered.

### Round 1 deferred item (a): per-group n on the figure — implemented

n is drawn as a row under the axis, positioned past the tick labels by an offset
computed from the base font size and the x-label angle (rotated labels are taller), with
the axis title pushed down to match.

It is only drawn when n actually **varies**. When every group has the same n, repeating
it under every bar is clutter and collides on narrow figures, so it is stated once in the
methods caption instead. Either way the caption names the n, and it is the *plotted* n —
the count that survived the `CFU > 0` filter, not the number of wells plated.

### Round 1 deferred item (b): effect sizes and confidence intervals — implemented

Every two-group comparison now reports `conf.low` / `conf.high` on the log10 difference,
the same interval back-transformed to `fold_change_low` / `fold_change_high`, and
`hedges_g`. Hedges' g rather than Cohen's d because CFU assays run at n = 3, where the
uncorrected d overstates the effect by roughly a third. Guarded against n < 2 and
zero variance (both return `NA`, not `Inf`).

### Round 1 deferred item (c): non-parametric option — implemented, with its limitation stated

Wilcoxon rank-sum is now selectable. It is reported honestly rather than sold as a fix:
**at n = 3 versus n = 3 the smallest attainable two-sided p is 0.1**, so no comparison can
reach 0.05. Rows computed under that condition carry a note, and the sidebar says so.

The point estimate is the Hodges-Lehmann shift returned by `wilcox.test`, not the
difference of medians — a median difference does not necessarily lie inside the interval
the same call reports.

### Round 1 deferred item (f): PowerPoint raster fallback DPI — fixed

`add_plot_slide()` used `input$animation_dpi` (GIF resolution, default 150) for the
non-`rvg` PNG fallback. It now uses the figure export DPI.

### The exported "reproducible R script" now actually reproduces the figure

The script carried a hand-maintained second copy of the plotting code that had already
drifted: it never applied `y_min`/`y_max`, tick spacing, minor ticks, plot theme
variants, or the coordinate system. It emitted a figure that did not match the app.

It now emits the app's own `make_cfu_plot()` and its helpers verbatim via `deparse()`, so
the two cannot diverge again. Verified by building both figures and comparing
`ggplot_build()` output layer by layer: **max difference 0.0 across all five layers**,
with identical y label, caption, and panel range.

That comparison also exposed a genuine reproducibility defect: the jittered replicate
points were unseeded, so re-exporting the same figure moved every point. Jitter now takes
a user-visible **Jitter seed** (default 1) passed to `position_jitter`/`position_jitterdodge`.

### Silently dropped rows are now declared

`prep_cfu_data()` filters `cfu > 0` because `log10` is undefined at or below zero.
Background-subtracted counts routinely land below zero — the gp75 dummy file loses
**13 of 72 rows**, dropping two groups to n = 1, where SD is undefined. The count and
reason now appear in a banner above the figure, not only in the QC tab.

### Smaller corrections

- Y-axis label was hardcoded to `CFU`. It is now a text input defaulting to `CFU/mL`, wrapped in `log10` automatically. `CFU`, `CFU/mL` and `CFU/plate` are different quantities.
- The methods caption ran off the page instead of wrapping. It is now wrapped against the real export width.
- `plot_summary()` used `case_when()` with scalar conditions, which dplyr 1.2 deprecated ("can result in subtle silent bugs"). Rewritten as `if`/`else`.
- The n-label layer initially anchored at `y = -Inf`, which `scale_y_log10` transforms to `NaN`, silently dropping every label on the raw-CFU-log-axis mode. Now anchored to a finite value.
- Figure QA reports size in both units and adds checks for the Y-axis quantity naming its denominator and for n being present on the figure.

## Still open (methodology choices, not defects)

1. **ANOVA sum-of-squares type.** `run_anova()` reports Type I sequential SS via `anova()`. The gp75 design is unbalanced after the `CFU > 0` filter, where Type II/III (`car::Anova`) is usually more appropriate. Changing this changes published numbers — the author's call.
2. **Normality diagnostics.** No residual QQ or Shapiro test is offered. The Wilcoxon option now exists as an alternative, but nothing tells the user when to reach for it.
3. **Paired / normalised readouts.** The app plots absolute counts. For an induction time-course the usual readout is survival — CFU at t120 relative to t0 **within the same replicate** — which normalises out plating variation. This needs a replicate-pairing model the data layer does not currently have.
4. **Upstream format gap.** CFU Calculator exports `sample` as one packed string (`"EV t0 12.5"`) with no separate time or dose column, so it cannot be loaded here without manual reshaping.

## Round 1 — earlier audit (unverified at the time, no R runtime)

Round 1 added the Okabe-Ito colorblind-safe palette (Wong 2011, *Nat Methods* 8:441) with
`#0072B2`/`#D55E00` as the default sample colors, and the automatic methods caption naming
the error-bar type, test, and correction. Both were confirmed working during round 2.
