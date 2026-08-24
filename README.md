# CFU Plot Studio

CFU Plot Studio is an R Shiny app for turning replicate-level colony forming unit data into publication-focused bar plots, summary tables, QC checks, and statistics.

The app is designed for microbiology lab workflows where data are usually collected as rows of replicate measurements across samples, treatments, timepoints, and CFU counts.

## What It Does

- Imports replicate-level CSV files without requiring a fixed column order.
- Maps sample/vector, treatment/dose/condition, timepoint, replicate, and CFU columns inside the app.
- Plots CFU summaries as bar plots or point plots with SD, SEM, 95% CI, IQR, or min-max variation intervals.
- Shows individual replicate points on top of bars.
- Draws the figure preview at the exact export geometry, so the font sizes on screen are the font sizes in the exported file.
- Runs replicate-level statistics on `log10(CFU)`, reporting the confidence interval, fold-change interval, and Hedges' g alongside each p value.
- Supports sample/vector comparisons, timepoint comparisons, treatment versus control, and all treatment-pair comparisons.
- Exports cleaned data, summary tables, QC tables, statistics, and ANOVA tables.
- Exports figures as high-resolution PNG, PDF, SVG, animated GIF, and PowerPoint.
- Can create editable PowerPoint vector figures when the `rvg` package is installed.
- Saves and reloads plot-style presets as JSON.
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
fill legend is hidden in that case (it would just list every bar), and Figure QA raises
it. Per-bar colour is unambiguous in the one-sample-one-timepoint mode, and elsewhere it
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
- Log10 CFU or raw CFU on a log axis.
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

## Data That Cannot Be Plotted

`log10` is undefined at or below zero, so rows with a non-positive or non-numeric CFU
value are excluded from the figure, the summary, and every test. Background-subtracted
counts can legitimately land below zero, so this is common. The app states how many
rows were dropped and why in a banner above the figure, and the plotted `n` is always
the surviving `n` — not the number of wells plated.

## Statistics

The default statistics use Welch t-tests on `log10(CFU)` values. The app also supports
Student t-tests, Wilcoxon rank-sum tests, and model-based marginal means through `emmeans`.

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

## Testing

```bash
Rscript tests/test_survival.R
Rscript tests/test_statistics.R
Rscript tests/test_column_matching.R
Rscript tests/test_end_to_end.R            # optionally: ... path/to/your.csv
```

`test_end_to_end.R` drives the real Shiny server with a real uploaded CSV, so it covers
the reactives rather than the helper functions. Point it at your own file to check the
whole pipeline against your data.

`test_statistics.R` re-derives each expected value from first principles or a hand-worked
example rather than from the app's own helpers, so a bug that is self-consistent across
the app still fails the test. Both scripts exit non-zero on failure.

Multiple-comparison correction options include:

- BH
- Holm
- Bonferroni
- None

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
- `tests/test_column_matching.R`: regression test for CSV header auto-detection.
- `dummy_cfu_example.csv`: synthetic example/template data.
- `outputs/`: local validation outputs and screenshots.

## License

This project is released under the MIT License.

## Citation And DOI

Citation metadata is provided in `CITATION.cff`. Zenodo metadata is provided in `.zenodo.json`.

After the repository is public on GitHub, connect it to Zenodo and create a GitHub release. Zenodo will archive that release and mint a DOI for it.

The initial release metadata lists Michael Baffour Awuah as maintainer. Update the affiliation field in `CITATION.cff` and `.zenodo.json` later if a formal institutional affiliation should be displayed.
