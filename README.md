# CFU Plot Studio

**Version 0.4.0.** Statistics from 0.1.x are *not* comparable with 0.2.0 and later — see
`CHANGELOG.md` and `AUDIT-2026-09.md` before re-using an older number.

CFU Plot Studio is an R Shiny app for turning replicate-level colony forming unit data into publication-focused bar plots, summary tables, QC checks, and statistics.

The app is designed for microbiology lab workflows where data are usually collected as rows of replicate measurements across samples, treatments, timepoints, and CFU counts.

## What It Does

- Imports replicate-level CSV files without requiring a fixed column order.
- Maps sample/vector, treatment/dose/condition, timepoint, replicate, and CFU columns inside the app.
- Plots CFU summaries as bar plots or point plots with SD, SEM, 95% CI, IQR, or min-max variation intervals.
- Shows individual replicate points on top of bars.
- Draws the figure preview at the exact export geometry, so the font sizes on screen are the font sizes in the exported file.
- Runs statistics on `log10(CFU)` at the level of the **biological** replicate, reporting the confidence interval, fold-change interval, and Hedges' g alongside each p value.
- Reports advisory spread and normality diagnostics on the same units the tests use.
- Censors counts at or below zero at a limit of detection, or excludes them, and records which.
- Supports sample/vector comparisons, timepoint comparisons, treatment versus control, and all treatment-pair comparisons.
- Exports cleaned data, summary tables, QC tables, statistics, and ANOVA tables.
- Exports figures as high-resolution PNG, PDF, SVG, animated GIF, and PowerPoint.
- Can create editable PowerPoint vector figures when the `rvg` package is installed.
- Saves and reloads plot-style presets as JSON.
- Keeps one style for a whole paper: colours belong to sample and timepoint names, one font
  family for every text, and a paper style that is applied every time the app starts.
- Exports an analysis manifest and a reproducible R script for the current figure.
- Includes a Figure QA checklist for publication-readiness checks.

## Data Format

Your CSV should contain one row per replicate measurement. Column names can vary because the app lets you map them.

Required information:

| Field | Example |
| --- | --- |
| Sample/vector | `Control strain`, `Test strain` |
| Treatment/dose/condition | `Baseline`, `Treatment A`, `Treatment B` |
| Timepoint | `Early`, `Late` |
| Replicate | `1`, `2`, `3` |
| CFU | `1200000` |

The included `dummy_cfu_example.csv` is synthetic example data and can be downloaded from the app as a template.

### Biological versus technical replicates

This is the single most consequential thing the app cannot work out from your file, so
it asks. **Biological replicate column** in the sidebar says which column identifies
*independent cultures*. Rows that share a value in that column are technical repeats —
the same culture plated more than once — and are averaged on the log10 scale (the
geometric mean of the plates) before any test runs. `n` then counts cultures, not plates.

By default the app uses the Replicate column for both, which is right when one row is
one culture. If your file has a separate plate or well column, point the biological
replicate selector at the culture column instead. If the file genuinely has no
independent replication, choose *None in this file* — the app will not invent one. It
labels the banner, the figure caption, the QC table and a `unit_of_replication` column
on every result **"technical replicates only"**, and those numbers must not be reported
as replicate-level statistics.

Before version 0.2.0 every unpaired test counted every row, so three cultures plated
four times each entered a t-test as `n = 12`. See `AUDIT-2026-09.md`.

Decimal-comma files (`1,5E+08` with `;` separators) and tab-separated files are read
too; the **CSV dialect** control sniffs the delimiter by default.

## Running The App

### Windows: double-click

Double-click **`Run CFU Plot Studio.bat`**. It finds R, installs anything missing into a
private `.Rlibrary` folder beside the app, picks a free port and opens your browser.
Close the console window to stop the app. Nothing is installed system-wide and nothing
leaves your machine.

The first run installs packages and can take several minutes. Later runs start in
seconds.

If R is installed somewhere the launcher does not look, point it at your `Rscript.exe`:

```powershell
setx CFU_RSCRIPT "C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
```

Don't have R? Install it from <https://cran.r-project.org/bin/windows/base/> first.

### Any platform: from a shell

```bash
Rscript run_app.R
```

`run_app.R` does the same dependency check and port selection, and works from any
working directory. Overrides:

| Variable | Default | Purpose |
| --- | --- | --- |
| `CFU_APP_HOST` | `127.0.0.1` | Bind address |
| `CFU_APP_PORT` | first free from 4267 | Fixed port; errors if that port is busy |
| `CFU_APP_LIB` | `<app>/.Rlibrary` | Where missing packages are installed |
| `CFU_NO_INSTALL` | unset | Set to `1` to fail rather than install anything |

### From an R session

```r
shiny::runApp(".")
```

This path assumes the packages below are already installed.

## Installation

The launchers handle this for you. To install by hand:

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "readr", "tibble", "tidyr", "scales",
  "emmeans", "broom", "DT", "colourpicker", "jsonlite"
))
```

Optional export packages. Each one only affects the export format named, and the app
reports which are absent at startup:

```r
install.packages(c(
  "officer",    # PowerPoint export
  "rvg",        # editable vector art inside PowerPoint
  "gganimate",  # animated GIF export
  "gifski"      # GIF encoding
))
```

## Figure Size

Width and height are entered in inches or millimetres; switching the unit converts the
numbers so the physical figure stays the same size. A readout under the size boxes
always states the geometry both ways plus the resulting pixel dimensions, for example
`3.50 x 2.76 in (89 x 70 mm) at 600 DPI = 2100 x 1656 px`.

The on-screen preview is rendered at that exact geometry rather than stretched to fill
the browser pane. Because the preview device resolution scales with it, an 8 pt tick
label occupies the same fraction of the figure on screen as it will in the exported
file — so a figure tuned against the preview does not need re-tuning after export.

Presets: single column (3.35 x 2.65 in), double column (7.0 x 4.2 in), square
(4.5 x 4.5 in), Nature single column (89 mm), Nature double column (183 mm). All set
600 DPI.

## Download Everything

**Download everything (.zip)** bundles the current figure and everything needed to
defend or rebuild it:

| File | What it is |
| --- | --- |
| `figure.png` / `.pdf` / `.svg` / `.pptx` | The figure in every static format |
| `legend_key.png` / `.pdf` / `.svg` | The figure's legend key on its own (see [Legend key](#legend-key)) |
| `legend_text.txt` | The legend text starter: each group with its colour and marker in words |
| `figure_reveal.gif` | Optional, off by default because it is slow |
| `data_cleaned.csv` | Every row that survived import |
| `data_plotted.csv` | The rows actually plotted — in survival mode the paired ratios, each with both source counts |
| `summary.csv` | Exactly what the Summary tab shows |
| `statistics.csv` | Every comparison with intervals, effect sizes and adjusted q values |
| `anova.csv` | The ANOVA table, naming its sum-of-squares type |
| `assumption_diagnostics.csv` | Per-cell spread and normality checks on the tested units (advisory) |
| `lod_wells.csv` | Present only when wells were censored: every affected well and what was done with it |
| `qc_replicates.csv`, `figure_qa.csv` | Source checks, replicate structure, publication checklist |
| `recreate_figure.R` | Standalone script that rebuilds the figure from embedded data |
| `plot_preset.json` | Every setting, reloadable from the sidebar |
| `analysis_manifest.json` | What was loaded, what was filtered, how the figure was made |
| `README.txt` | Contents, the figure geometry, the readout, and what could not be produced |

Each item is attempted independently, so a missing optional package costs that one file
rather than the whole archive, and the README says which are absent and why.

**The bundle names the code that produced it.** `analysis_manifest.json`, `README.txt` and
the header of `recreate_figure.R` all carry the app version, plus the short git SHA when
the app is run from a checkout. A bundle that cannot identify its own version is not
reproducible, whatever else it contains.

**A figure file is verified after it is written.** `ggsave` reports a graphics device that
failed to open as a *warning*, not an error, so a download could previously hand back an
empty file and the bundle could lose `figure.pdf` in silence — `capabilities("cairo")`
returns `TRUE` on exactly the machines where this happens (macOS without XQuartz). Every
export now checks that the file exists, is non-empty, and begins with the magic bytes of
the format requested, and falls back from `cairo_pdf` to the base `pdf` device with a
visible note about font embedding. A format that cannot be written at all is reported as
missing rather than shipped as a zero-byte file.

## Legend Key

Every figure comes with a **legend key**: a small image listing each plotted sample or
group with its exact colour, marker and outline, next to its name, in the figure's font.
It sits in the **Legend key** box directly under the PNG / PDF / SVG buttons:

- **Key PNG / Key PDF / Key SVG** download the key in the same formats as the figure. In
  PDF and SVG the names stay editable text.
- **Legend text starter** is a read-only box with a sentence ready to paste into the figure
  legend, and a **Copy** button. Its format is
  `NAME (colour, marker); NAME (colour, marker); ...`, followed by the outline colour, e.g.

  ```
  MG1655 (black, filled circles); MG1655 + WT (orange, filled circles). Circles are outlined in black.
  ```

  Colour words are the nearest plain name (by CIE Lab distance); two groups that would get
  the same word are told apart with "light" / "dark". Bar figures say `bars`, or
  `bars, replicates as filled circles` when replicate points are shown.
- **Place key on figure** draws the same key on the figure itself, at the **Legend
  position** you chose (the right-hand margin if that is *none*). It also gives the
  one-sample figure and per-bar colouring a key, which they otherwise do not show.

The key is the figure's own legend cut out of the figure, and the text is read from the
same colour mapping the figure is drawn from, so both always follow the current sample
names, colours and selection, and the key on the figure and the downloaded key are the
same. It covers every plot mode, the PowerPoint and reveal slides and the GIF (through
**Place key on figure**), **Download everything** (`legend_key.*` and `legend_text.txt`)
and `recreate_figure.R`, which rebuilds the key and the text alongside the figure.

## One Style For The Whole Paper

A paper's figures come from several CSVs and several sessions; they should still look
like one set.

- **Colours belong to names.** Under **Apply Okabe-Ito colorblind palette** there is one
  colour picker per sample and one per timepoint in the loaded data, labelled with its
  name. The colour is stored against the NAME, not the position: load another CSV and
  every name the app has seen keeps its colour, wherever it falls in the level order,
  and a new name takes the first default colour no other name is using. Samples default
  to Okabe-Ito in order (blue, vermillion, bluish green, ...); timepoints to the two
  blues the app has always used, then Okabe-Ito. Presets, the manifest and
  `recreate_figure.R` carry the colours as `sample_palette` / `time_palette` (name ->
  hex). Loading a preset merges them by name, keeping names the current CSV lacks, so
  one preset can name every strain of the paper. A preset saved before 0.4.0, which
  numbered its colours (`sample_color_1`, `sample_color_2`, `time_color_1`,
  `time_color_2`), gives them to the first and second name loaded, and says so.
- **Font.** **Font** (above **Base font size**) sets the family of every text on the
  figure: titles, axes, legend, caption, statistic labels, the n row, the legend key and
  every export. *Custom…* takes any installed family by name. A family this machine does
  not have is drawn in the default sans; the app says so once, and Figure QA carries a
  **Font** row with the font requested and the font used. The manifest records both.
- **Paper style.** In **Reproducibility**, **Save as paper style** writes today's
  settings and colours to `paper_style.json` beside `app.R` (or wherever the
  `CFU_PAPER_STYLE` environment variable points), and the app applies that file every
  time it starts; the line under the buttons names the file and when it was saved.
  A style is a style, so it leaves the settings that belong to one figure alone: the
  plot title, plot mode, y minimum, y maximum, survival baseline, survival readout and
  the statistics shown. **Forget paper style** deletes the file after asking; nothing
  else changes. The file has the shape of a plot preset, so it can also be loaded
  through **Load plot preset JSON**, which, as for any preset, applies everything.

## Keeping The Data Area Fixed

ggplot sizes the panel last: it gets whatever space is left after the legend, the titles,
the axis labels and the caption have taken theirs. Two figures exported at the same width
therefore have different-sized data areas the moment their labelling differs. Measured on
a 7 x 4.4 in figure from this app, moving the legend from the top to the right side
shrinks the panel from 6.39 in wide to 3.83 in — a 40% loss of data area with the data
untouched. Panels meant to be compared side by side cannot be built that way.

Set **Width and height describe** to *The plot panel only* and the priority inverts: the
data area is pinned to exactly the size you asked for and the figure grows around it. The
readout then shows both, for example

```
Panel pinned at 4.20 x 2.60 in (107 x 66 mm)  ->  figure 6.18 x 4.27 in ...
```

Two survival figures from the same data, one plain and one with a side legend, a long
title and angled axis labels, both come out with a panel of exactly 4.200 x 2.600 in while
their figures differ (4.80 x 4.68 in versus 6.18 x 4.27 in). That is the point: the
comparable thing stays constant and the rest gives way.

Notes:

- With facets, **each** panel gets the size, so one facet of a multi-panel figure is
  directly comparable with a single-panel figure at the same setting.
- The journal size presets are figure widths. In panel mode they set the panel that
  produces that figure, rather than silently reinterpreting an 89 mm figure as an 89 mm
  panel.
- PNG, PDF, SVG, the preview and PowerPoint all honour the pinned panel. Animated GIF does
  not — gganimate builds its frames from the ggplot rather than the laid-out table — and
  the app says so when you export one.
- The default is still *The whole figure*, and that path is unchanged.

## Bars Or Points

A bar encodes its value as a length measured from zero. On a `log10(CFU)` axis that
baseline is 1 CFU/mL, which is arbitrary, and viable counts spanning 10^8 to 10^11
leave most of the panel empty. Setting **Mean shown as** to *Points (no bars)* draws
the group mean as a marker instead, which allows the axis to be framed on the data.
*Auto y-axis* respects the distinction: it keeps the zero baseline for bars and fits
the range to the data for points.

## Colouring Bars Individually

**Bar colouring** switches between one colour per group (the default) and one colour per
bar. In per-bar mode you get a colour picker for every drawn bar, plus a button that
lays Okabe-Ito across them and one that returns to the group colours.

A caveat the app enforces rather than hides: when bars are dodged by sample or
timepoint, colouring them individually means colour no longer identifies the group. The
fill legend is hidden in that case (it would just list every bar) unless **Place key on
figure** is on, and Figure QA raises it. Per-bar colour is unambiguous in the one-sample-one-timepoint mode, and elsewhere it
is best used to highlight rather than to classify.

## Moving Things On The Canvas

The **Canvas** bar above the figure has three modes:

- **Off** — clicks do nothing.
- **Place legend** — click anywhere on the figure and the legend centre moves there.
- **Move stat labels** — click a significance label to pick it up, click again to drop it.

**Reset placement** returns the legend and every label to its automatic position.
Nudges are stored per label in axis units, so they survive re-rendering, filtering,
resizing, preset save/load, and the reproducible-script export.

Title, subtitle, methods caption and both axis titles have alignment sliders under
**Text placement** (0 left, 0.5 centred, 1 right).

## Paired Survival

An induction time-course is not really a question about absolute counts, it is a question
about survival: how much of each culture is left at the readout timepoint relative to
where that same culture started. **Plot mode > Paired survival** computes, for every
(sample, treatment, replicate) that has BOTH timepoints:

```
log10 survival ratio = log10(CFU at readout) - log10(CFU at baseline)
```

then summarises those per-replicate ratios. Pick the baseline and readout timepoints in
the sidebar; a dashed reference line marks no change.

**Why pair.** When every replicate has both timepoints the paired point estimate is
algebraically identical to the unpaired one — `mean(a - b)` is `mean(a) - mean(b)`. What
pairing buys is the uncertainty: the standard error becomes the spread of *within-culture*
differences rather than the pooled spread across cultures. On replicates whose starting
titres differ by orders of magnitude that is the difference between p = 0.002 and p = 0.5
on the same estimate.

And when a replicate is missing one timepoint, the two stop agreeing entirely. The
unpaired calculation subtracts a baseline mean containing a replicate the readout mean
cannot contain, charging that replicate's whole titre to the treatment. On the bundled
gp75 dummy file that flips the sign of the biology at one dose: paired says 48% survival,
unpaired says 119% growth.

**Axis.** The three display scales — log10 ratio, fold change, percent of baseline — are
the same numbers with different tick labels. Switching never rescales or distorts
anything. Percent is offered but is intrinsically asymmetric (a 10x drop is 10%, a 10x
rise is 1000%), so log10 is the default. The raw-CFU log axis is not available in this
mode: the plotted quantity is already a log ratio.

**Statistics.** The default test asks whether survival differs from no change, as a
one-sample t-test of the per-replicate log ratios against zero — which is the paired test.
The reported effect size is `d_z` (mean of differences over their SD), reported under its
own column name because it is a different quantity from the two-sample Hedges' g and the
two must not be compared. Between-construct and dose-versus-control comparisons run on the
same ratios. The timepoint comparison is refused, because both timepoints have already
been consumed to form the ratio.

**Pairing between constructs and between doses.** The within-culture pairing above is
guaranteed by the design: the two timepoints came from one flask. Whether replicate 1 of
one construct and replicate 1 of another are also the same experiment — one split culture,
one day — is a claim the CSV cannot settle, so **Replicate labels match across samples and
treatments** is off by default.

Tick it and the between-construct and dose-versus-control comparisons become paired
t-tests on the matched differences. When the claim is true this is markedly more
sensitive: on three days whose baselines span four logs, a consistent half-log construct
effect gives p = 0.002 paired and p = 0.37 unpaired, from the same point estimate. When
the claim is false, ticking it invents a pairing and the p values are wrong.

Pairing is not free. A replicate with no counterpart on the other side is excluded, and
the Statistics tab reports `n_matched` per row plus a note naming how many were dropped.
On the bundled gp75 dummy file pairing actually *reduces* the number of testable doses
from three to two, because different replicate numbers survived on each side — which is
exactly the sort of thing worth seeing before trusting the result. The rank test has no
paired form here, so selecting both leaves the comparison unpaired rather than silently
substituting a signed-rank test.

**What pairing costs, stated.** Wells whose partner did not survive the `CFU > 0` filter
contribute nothing, and a cell can end up with no pairs at all and vanish from the figure.
The banner above the plot and the Figure QA "Pairing completeness" check report the
complete pairs, the orphans, any unlabelled wells, and name any cell that disappeared. The
plotted n counts **pairs, not wells**.

## Publication Figure Controls

The app includes controls for:

- Exact export width, height, and DPI, in inches or millimetres.
- Reproducible size presets for single-column, double-column, square, and Nature-width figures.
- A custom Y-axis quantity (`CFU/mL`, `CFU/plate`, `CFU/OD600`), wrapped in log10 automatically.
- A replicate-n row under the axis when n differs between groups, or a single n statement in the methods caption when it does not.
- A jitter seed, so replicate points land in the same place on every re-export.
- Log10 CFU, or raw CFU on a log axis. Both summarise on the log10 scale — the scale
  every test uses — so the raw-axis bar is the **geometric mean** with a multiplicative
  error interval, and the caption says so.
- Manual y-axis minimum and maximum.
- Major and minor y-axis tick spacing.
- Major and minor y-axis guide lines.
- Plot box, axis line width, tick length, and minor tick length.
- Bar width, dodge width, outline width, error-bar width, point size, point alpha, and jitter.
- Capped error bars, uncapped whiskers, mean point plus whiskers, mean crossbar intervals, or replicate-points-only variation display.
- Font sizes for base text, title, subtitle, and statistic labels.
- Legend position, including inside-plot positioning.
- Custom colors for samples, timepoints, bars, outlines, axes, grids, and statistic labels.
- Treatment units, time units, and optional unit suffixes.

## Data That Cannot Be Plotted, And The Limit Of Detection

A non-numeric or blank CFU value cannot be plotted or tested and is excluded from the
figure, the summary, and every test. The app states how many rows were dropped and why in
a banner above the figure, and the plotted `n` is always the surviving `n` — not the
number of wells plated.

A count of **zero** is different: `log10` is undefined there, but the reading is a real
measurement — below the limit of detection — and it happens exactly where a treatment
worked. Excluding those wells biases a log-reduction *towards the null*. On a control at
1e8 against a treated arm of 1e2, 1e2 and 0, dropping the zero reports a 6.0-log reduction
where censoring it at 1 CFU/mL gives 6.67. Background-subtracted counts can legitimately
land below zero as well, and are treated the same way.

**Counts at or below zero** in the sidebar offers the two defensible treatments:

- *Exclude the well* — the default, and what every version before 0.2.0 did.
- *Substitute at the limit of detection* — set your LOD (default 1 CFU/mL, the smallest
  countable titre on a neat plate) and substitute at the LOD or at LOD/2.

Whichever you choose, the rule, the count and **every affected well** are written into
the figure caption, the Figure QA tab, `analysis_manifest.json`, and `lod_wells.csv`
inside the export bundle. The choice does not survive only as a banner on screen.

## Statistics

The default statistics use Welch t-tests on `log10(CFU)` values. The app also supports
Student t-tests, Wilcoxon rank-sum tests, and model-based marginal means through
`emmeans`, with optional Tukey HSD or Dunnett-versus-control post-hoc procedures.

**Every test runs on one value per biological replicate.** Technical repeats are
collapsed first, on the log10 scale, and each result row names its
`unit_of_replication` and the `n_a` / `n_b` it actually compared.

Each comparison reports the log10 difference with its confidence interval, the same
interval back-transformed to a fold-change range, and Hedges' g (Cohen's d with the
small-sample correction, which matters at n = 3).

The rank test is offered because `log10(CFU)` normality is an assumption, not a fact —
but note that with three versus three replicates the smallest attainable two-sided p is
0.1, so no comparison can reach 0.05. Rows produced under that condition carry a note
saying so. It uses the exact null distribution unless ties force the normal
approximation, which is also reported per row.

Every pair is tested. With three or more samples or timepoints the figure annotates only
the first pair — stacked stars at one x position are unreadable — but all pairs are
computed, corrected together, named in the caption, and exported in the Statistics tab.

The ANOVA uses Type II sums of squares via `car::Anova` when `car` is installed, because
the `CFU > 0` filter usually leaves the design unbalanced and Type I sequential SS would
make each main effect depend on the order the terms appear in the formula. The table
names the type it used.

### How the ANOVA relates to the pairwise table

It depends on which test you picked, so the ANOVA tab states it in words rather than
leaving the layout to imply it:

- **Welch or Student t-tests** — the pairwise table is an *independent family*, not a
  post-hoc of the ANOVA. Its correction is applied across that family alone, and a
  non-significant omnibus does not gate it. This is a legitimate standalone procedure,
  but it is not protected post-hoc testing and should not be described as such.
- **emmeans with Tukey or Dunnett** — the pairwise table *is* a protected post-hoc
  family on the model the ANOVA reports. Those procedures control the family-wise error
  themselves, so the app's own correction setting is disabled and the table records
  `p_adjust_method = none` rather than correcting twice.
- **Wilcoxon** — the omnibus becomes a Kruskal-Wallis rank-sum test, so the two tables
  describe the same kind of procedure. (Dunn's post-hoc is not implemented; the pairwise
  family remains Wilcoxon tests with the chosen correction.)

### Assumption diagnostics

The QC tab reports per-cell `n`, mean and SD on `log10(CFU)`, a Shapiro-Wilk p where one
is defined, and each cell's variance as a ratio to the narrowest cell — all computed on
the same aggregated units the tests use. A note appears when you have selected Student,
or a pooled-variance `emmeans` model, and the widest cell has more than 4x the variance
of the narrowest.

These are **advisory**. Nothing switches a test and nothing is gated on a p-value: at
n = 3 Shapiro-Wilk has almost no power, so a passing p is not evidence of normality.

### Which mean the summary table reports

`summary.csv` and the Summary tab report both centres, named so they cannot be confused:

- `geometric_mean_cfu` with `geometric_sd_factor` — the quantity the figure draws and the
  tests analyse. The interval is *multiplicative*: geometric mean ×/÷ the factor, given as
  `geometric_mean_low` and `geometric_mean_high`.
- `arithmetic_mean_cfu`, `arithmetic_sd_cfu`, `arithmetic_sem_cfu` — kept because some
  readers want them, but they are not what was tested. On three wells at 1e6, 1e7 and 1e8
  the arithmetic mean is 3.7e7 against a geometric mean of 1e7.

Before 0.2.0 these were `mean_cfu` / `sd_cfu` / `sem_cfu` sitting beside
`geometric_mean_cfu`, with nothing but the word to tell them apart and no dispersion at
all on the geometric side.

## Testing

```bash
Rscript tests/test_bundle.R                # optionally: ... path/to/your.csv
Rscript tests/test_panel_size.R
Rscript tests/test_survival.R
Rscript tests/test_statistics.R
Rscript tests/test_column_matching.R
Rscript tests/test_end_to_end.R            # optionally: ... path/to/your.csv
Rscript tests/test_legend_key.R
Rscript tests/test_paper_style.R
```

`test_end_to_end.R` drives the real Shiny server with a real uploaded CSV, so it covers
the reactives rather than the helper functions. Point it at your own file to check the
whole pipeline against your data.

`test_statistics.R` re-derives each expected value from first principles or a hand-worked
example rather than from the app's own helpers, so a bug that is self-consistent across
the app still fails the test. Both scripts exit non-zero on failure.

`test_legend_key.R` checks the legend key from the files the app writes: every group in
the figure's colour mapping appears in the key with the same colour, marker and name;
renaming, recolouring and deselecting a sample are followed; the text starter names
every group with a colour word; the key placed on the figure matches the downloaded one;
names stay text in SVG and PDF; and the bundled `recreate_figure.R` rebuilds the key in
a fresh R session.

`test_paper_style.R` renders the default figure in every plot mode with the 0.3.0 code
(`git show 0762ca1:app.R`, the 0.3.0 commit) and with the current code, in separate R processes,
and checks the SVG files are byte-identical. It then checks that colours follow sample
names across CSVs and through presets (old numbered presets included), that the font
reaches every text node, the key and a pinned panel, that the paper style is saved,
auto-loaded without its figure-specific settings and forgotten, and that the bundled
`recreate_figure.R` reproduces the palette and the font in a fresh R session.

Multiple-comparison correction options include:

- BH
- Holm
- Bonferroni
- None

The **scope** of that correction — across every comparison in the figure, or within each
panel or group — is honoured by all of the t-test, rank-test and `emmeans` paths, and the
scope actually used is recorded in the `adjustment_scope` column of `statistics.csv`.
(Before 0.2.0 the `emmeans` path silently corrected globally whatever the control said.)
Tukey and Dunnett are the exception, and say so: they control the family-wise error
themselves, so the app's correction is disabled rather than applied on top.

Statistic labels can be shown as significance stars or exact adjusted values.

## Notes For Manuscripts

For manuscript figures, a good starting workflow is:

1. Upload or map the data.
2. Check the QC tab for replicate issues.
3. Choose the plot mode and statistics comparison.
4. Set y-axis boundaries and tick spacing.
5. Use a reproducible figure size preset.
6. Export SVG/PDF for vector editing or editable PowerPoint if `rvg` is installed.
7. Export the statistics table alongside the figure for record keeping.
8. Save the plot preset, analysis manifest, and reproducible R script with the project folder.

## Files

- `app.R`: main Shiny application.
- `Run CFU Plot Studio.bat`: double-click launcher for Windows.
- `run_app.R`: cross-platform launcher (dependency check, free-port selection, browser).
- `tests/test_column_matching.R`: regression tests for CSV header auto-detection and CSV dialects.
- `dummy_cfu_example.csv`: synthetic example/template data.
- `CHANGELOG.md`: release history, including which versions produce comparable statistics.
- `AUDIT-2026-09.md`: the September 2026 statistical audit and what was done about each finding.
- `outputs/`: local validation outputs and screenshots.

## License

This project is released under the MIT License.

## Citation And DOI

Citation metadata is provided in `CITATION.cff`. Zenodo metadata is provided in `.zenodo.json`.

After the repository is public on GitHub, connect it to Zenodo and create a GitHub release. Zenodo will archive that release and mint a DOI for it.

The initial release metadata lists Michael Baffour Awuah as maintainer. Update the affiliation field in `CITATION.cff` and `.zenodo.json` later if a formal institutional affiliation should be displayed.
