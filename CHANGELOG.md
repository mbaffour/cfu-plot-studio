# Changelog

All notable changes to CFU Plot Studio. Versions follow [semantic versioning](https://semver.org/).

Entries marked **STATISTICS CHANGE** alter numbers this app has previously produced.
Figures and p-values from a version before such a change are not comparable with figures
and p-values from a version after it.

## 0.4.0 — 2026-11-09

One style for the whole paper. No statistic, summary or plotted value changes, and
figures with every setting at its default render byte-identically to 0.3.0
(`tests/test_paper_style.R` renders the demo data in all four plot modes with the 0.3.0
code and this code, in separate R processes, and compares the SVG files and legend keys
byte for byte).

### Added

- **Colours by sample and timepoint NAME.** The two numbered sample pickers and the two
  timepoint pickers ("0 min color", "120 min color") are replaced by one picker per
  sample and one per timepoint in the loaded data, labelled with the name. Colours live
  in a per-session colour registry keyed by name (`extend_colour_registry()`,
  `registry_colours()`): loading another CSV keeps every name's colour and gives a new
  name the first default no other name uses. `figure_key_mapping()` reads the registry,
  so the figure, the legend key and the text starter follow it; per-bar colours seed
  from it. **Apply Okabe-Ito** assigns Okabe-Ito in level order to the names loaded.
  Presets, the manifest and `recreate_figure.R` carry `sample_palette` and
  `time_palette` (name -> hex); loading a preset merges them by name and keeps names the
  current CSV lacks.
- **Font family.** A **Font** menu (Default sans, Arial, Helvetica, Times New Roman,
  Serif, Custom…) above **Base font size**, threaded through both themes, every
  `element_text()` and both `geom_text()` layers (statistic labels and the n row), so
  the legend key, PowerPoint, reveal slides and GIF follow. `resolve_font_family()`
  checks a named font with systemfonts (optional) and falls back to sans when the
  machine does not have it; the app says so once, and Figure QA has a **Font** row
  (requested / used). Saved in presets, the manifest (requested and used) and
  `recreate_figure.R`.
- **Paper style.** **Save as paper style** writes the current settings and colour
  registry to `paper_style.json` beside `app.R` (`CFU_PAPER_STYLE` overrides the path),
  in the shape of a preset with `"kind": "paper_style"`. It is applied at every start
  through the same code as a preset upload (`apply_preset_settings()`), except the
  settings that belong to one figure (`figure_specific_setting_ids`: plot title, plot
  mode, y minimum, y maximum, survival baseline, survival readout, statistics shown).
  **Forget paper style** deletes the file after a confirmation. `paper_style.json` is
  git-ignored.
- `tests/test_paper_style.R`.

### Changed

- **A figure with three or more samples or timepoints** now colours the third and later
  groups with Okabe-Ito (bluish green, reddish purple, orange, ...) instead of
  `hcl.colors("Dark 3")`. One- and two-group figures are unchanged. "Dark 3" is used only
  past eight samples (ten timepoints).
- Deselecting a sample no longer shifts the others' colours: colour follows the name.

### Compatibility

- A preset saved before 0.4.0 still loads. Its `sample_color_1` / `sample_color_2` go to
  the first and second sample loaded at the time, `time_color_1` / `time_color_2` to the
  first and second timepoint, and a notification says so. A preset that carries
  `sample_palette` / `time_palette` uses those instead.

### Fixed

- **Colour settings in presets were never restored.** The axis, guide-line, outline,
  single-bar and statistic-label colours fell into the slider residual of
  `slider_setting_ids` and were sent through `updateSliderInput(as.numeric("#262626"))`.
  They now have their own list, `colour_setting_ids`, and `updateColourInput()`.
- **A preset in millimetres no longer has its size converted twice.** Applying
  `size_units` with the width and height made the unit converter treat the preset's
  numbers as inches; the converter is now told the preset's units are in force.
- **`recreate_figure.R` no longer draws on Rscript's default `pdf()` device**, which
  cannot draw a system font such as Times New Roman (it stopped with "failed to find or
  load PDF CID font" on the log10 axis title). It draws to the screen only in an
  interactive session; the PNG files it writes are unchanged.

## 0.3.0 — 2026-11-08 (released together with 0.4.0)

The legend key. No statistic, summary or plotted value changes: figures with the new
switch off render byte-identically to 0.2.0 (checked on SVG output in every plot mode).

### Added

- **Legend key for every figure.** Under the figure's PNG / PDF / SVG buttons: **Key
  PNG / Key PDF / Key SVG**, a read-only **Legend text starter** with a **Copy** button,
  and a **Place key on figure** switch.
  - The standalone key is the figure's own legend, cut out of the figure built with the
    key switched on (`legend_key_grob()`), so its glyphs, exact colours, outline, names
    and font are the ones drawn on the figure. Names are live text in SVG and PDF. It is
    written through `ggsave_checked()`, so a failed write raises rather than shipping an
    empty file.
  - **Place key on figure** draws that same legend on the figure at the chosen legend
    position (the right margin when the position is *none*). It is how the one-sample
    figure and per-bar colouring, which normally show no legend, get a key.
  - The text starter reads `NAME (colour, marker); ...` plus the outline colour. Colour
    words are the nearest entry of a fixed 27-name table by CIE Lab distance, with
    "light" / "dark" added only when two groups in one figure would share a word.
  - The switch is saved in plot presets, the manifest and `recreate_figure.R`.
- **One colour mapping per figure.** `figure_key_mapping()` is now the single place a
  figure's groups get their fill colours; `make_cfu_plot()` passes its values to
  `scale_fill_manual()` in every mode, and the key and text starter are read from it.
- `tests/test_legend_key.R`: from the files the handlers write, on a three-sample fixture.

Figures the app makes, and how each gets its key:

| Figure | Encodes groups by | Key |
| --- | --- | --- |
| Main figure, *combined* mode (PNG / PDF / SVG / preview) | fill per sample | standalone key + switch |
| Main figure, *survival* mode | fill per sample | standalone key + switch |
| Main figure, *sample_both* mode | fill per timepoint | standalone key + switch |
| Main figure, *sample_time* mode | one sample, one colour | standalone key (one entry) + switch |
| Any mode with per-bar colouring | fill per bar | standalone key (one entry per bar) + switch |
| PowerPoint slide | same figure | embedded via the switch |
| Reveal slides (PPT), one per step | same figure, groups revealed so far | embedded via the switch |
| Animated GIF | same figure | embedded via the switch (not panel-pinned, as before) |
| Download everything (.zip) | — | `legend_key.png/.pdf/.svg` + `legend_text.txt` |
| `recreate_figure.R` | — | rebuilds `legend_key_recreated.png` + `legend_text_recreated.txt` |

The app has no other figures: the QC, assumption-diagnostics, statistics and ANOVA tabs
are tables. There is no per-group marker shape or line type to key — every group is a
bar or a shape-21 circle with the same outline — so the key varies by fill only.

### Fixed

- **`recreate_figure.R` now runs in a fresh R session.** It called `stats_caption()`,
  which since 0.2.0 needs `is_emmeans_method()` and `emmeans_adjust_for()`, without
  carrying them, so any figure with statistics failed; a figure with no statistic labels
  failed on `read.csv()` of the empty annotation table. `test_bundle.R` did not see
  either because it runs the script inside a session that already holds the app's
  helpers; `test_legend_key.R` runs it in a separate `Rscript` process.

## 0.2.0 — 2026-09-14

The 2026-09 statistical audit and its fixes. Every finding and its disposition is in
`AUDIT-2026-09.md`.

### Comparability with 0.1.x

**Statistics from 0.1.x are NOT comparable with 0.2.0.** Two changes move published-looking
numbers:

- Any file that carries technical plate replicates will produce different p-values,
  confidence intervals, effect sizes and degrees of freedom, because `n` now counts
  biological replicates rather than plates. p-values generally get **larger** — the old
  ones were inflated. A file with exactly one row per biological replicate is unaffected.
- Everything drawn on the **Raw CFU on log axis** mode changes: the bar is now the
  geometric mean rather than the arithmetic mean, and the error bar is multiplicative.
  The default `log10(CFU)` axis is unchanged.

If you need to reproduce an older figure exactly, check out the 0.1.0 tag; the version is
now stamped into every export bundle so an existing bundle can be traced to the code that
made it.

### Changed — STATISTICS CHANGE

- **Technical replicates no longer inflate `n`** (CPS-1). The data mapping asks which
  column identifies the *biological* replicate. Rows sharing that value are technical
  repeats and are averaged on the log10 scale — the geometric mean of the plates, the
  scale every test runs on — before the pairwise tests, the ANOVA, the `emmeans` model and
  survival pairing. Previously `run_two_group_test()` pulled every row, so three
  biological replicates plated four times each entered a t-test as `n = 12` on 22 df; on a
  0.4-log difference that reports p = 9e-11 where the honest test on the three biological
  means gives p = 0.008. The paired survival path had always collapsed within a replicate
  label first, so the two paths disagreed by construction about what a replicate was.
  A file with no such column is not guessed at: the banner, the caption, the QC table and
  a `unit_of_replication` column on every result all say "technical replicates only".
  `n_a` / `n_b` now report the units actually compared.
- **"Raw CFU on log axis" summarises on the log scale** (CPS-2). That mode previously
  summarised on the raw scale while the statistics and the caption both said log10, so on
  wells at 1e6 / 1e7 / 1e8 the bar sat at 2.35e7 — 2.35x the geometric mean the test
  analysed — `mean - SD` came out negative, and the lower whisker was silently clamped to
  machine epsilon, drawing an error bar across some twenty decades. Both axis modes now
  summarise on log10; the raw mode back-transforms, giving a geometric mean with a
  multiplicative interval that is positive by construction. The caption names the centre
  statistic and the scale instead of an unqualified "mean ± SD".
- **The correction scope is honoured by the `emmeans` path** (CPS-5). `run_contrast()`
  hardcoded a global adjustment and attached no panel column, so the "Within each
  panel/group" control was inert for that method. The `emmeans` `by` variables are now the
  panel and the user's scope is applied.

### Added

- **Limit-of-detection substitution** (CPS-3). A plate with no colonies is a measurement
  below the limit of detection, not a gap, and dropping it biases a log-reduction *towards
  the null*: on control 1e8 x3 against treated 1e2, 1e2 and 0, dropping the zero reports
  6.0 logs where censoring at 1 CFU/mL gives 6.667. **Counts at or below zero** in the
  sidebar now offers substitution at a user-set LOD (default 1 CFU/mL), at the LOD or at
  LOD/2, against the unchanged default of exclusion. Censored wells are marked in the
  plotted frame, counted in the summary, named in the figure caption and in a new
  "Non-positive counts" figure-QA check, and listed well by well in
  `analysis_manifest.json` and in `lod_wells.csv` inside the bundle. A biological
  replicate is marked censored if any of its plates was.
- **Tukey HSD and Dunnett-versus-control post-hoc procedures** on the `emmeans` model
  (CPS-7), for users who want the classical protected route. They control the family-wise
  error themselves, so the app's own correction is disabled for them and the table records
  `p_adjust_method = none` rather than correcting twice. Dunnett degrades rather than
  mislabelling an all-pairs family.
- **A Kruskal-Wallis omnibus** for the rank-test family (CPS-7), so the omnibus and the
  pairwise table describe the same kind of procedure. Dunn's post-hoc is not implemented.
- **A statement of what the ANOVA is** (CPS-7). The ANOVA tab now says in words whether the
  pairwise table is a protected post-hoc family or an independent one, rather than leaving
  the layout to imply the former.
- **Assumption diagnostics** (CPS-6). The QC tab carries a per-cell table of `n`, mean and
  SD on `log10(CFU)`, a Shapiro-Wilk p where one is defined, and each cell's variance as a
  ratio to the narrowest, computed on the aggregated units the tests use. A note appears
  when Student or a pooled-variance `emmeans` model is selected and the widest cell has
  more than 4x the variance of the narrowest. Deliberately advisory: nothing switches a
  test and nothing is gated on a p-value, because at n = 3 Shapiro-Wilk has almost no
  power. Exported as `assumption_diagnostics.csv`.
- **A CSV dialect control** (CPS-9) with comma, semicolon/decimal-comma and tab options,
  defaulting to sniffing the delimiter from the first few lines. A European export
  (`1,5E+08` with `;` separators) previously parsed to a character column, then to `NA`,
  and was dropped wholesale by the `!is.na(cfu)` filter. The dialect used is recorded in
  the manifest.
- **`APP_VERSION`** (CPS-8), surfaced in the UI header and written into
  `analysis_manifest.json`, the bundle `README.txt` and the `recreate_figure.R` header,
  with the short git SHA when the app runs from a checkout. A bundle that cannot name the
  code that produced it is not reproducible, which is the whole claim of that export.
- **Geometric dispersion in `summary.csv`** (CPS-10): `geometric_sd_factor`,
  `geometric_mean_low` and `geometric_mean_high`, a multiplicative interval around the
  geometric mean, which previously carried no dispersion at all.
- **A "Unit of replication" figure-QA check** (CPS-1).

### Fixed

- **`cairo_pdf` exports silently wrote nothing** (CPS-4). `ggsave()` reports a graphics
  device that failed to open as a *warning*, not an error, so on macOS without XQuartz the
  PDF download handed the browser an empty file and the export bundle lost `figure.pdf`
  without saying so. `capabilities("cairo")` returns `TRUE` on exactly those machines, so
  it is not a usable guard. `ggsave_checked()` now asserts the post-condition instead — the
  file must exist, be non-empty and start with the magic bytes of the format requested —
  and falls back from `cairo_pdf` to the base `pdf` device with a visible note about font
  embedding. A format that cannot be written at all raises, so the bundle records it as
  missing rather than shipping a zero-byte file.

### Renamed

- `mean_cfu` / `sd_cfu` / `sem_cfu` in `summary.csv` are now `arithmetic_mean_cfu` /
  `arithmetic_sd_cfu` / `arithmetic_sem_cfu` (CPS-10). On three wells at 1e6, 1e7 and 1e8
  the arithmetic mean is 3.7e7 against a geometric mean of 1e7, and the old names sat
  beside `geometric_mean_cfu` with nothing but the word to tell them apart. **Any script
  reading those three columns by name needs updating.**

### Tests

- `tests/test_statistics.R` gained coverage of the unit of replication, the back-transformed
  plot summary, the `emmeans` panel and scope, Tukey and Dunnett, the LOD rules, the summary
  column names and the assumption diagnostics. Each case re-derives its expected value from
  first principles rather than from the app's own helpers.
- `tests/test_bundle.R` previously asserted only that `figure.pdf` was *in* the archive,
  which the CPS-4 defect satisfied with an unreadable file; it now checks the headers of all
  three figure formats and the version stamps.
- `tests/test_panel_size.R` covers `ggsave_checked()` directly, including a device chain that
  cannot work.
- `tests/test_column_matching.R` covers the CSV dialects.

## 0.1.0 — 2026-09-12

Initial public release. See `RELEASE_NOTES_v0.1.0.md` for the full list.
