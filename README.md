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

## Installation

Install R, then install the core packages:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "readr",
  "emmeans",
  "broom",
  "DT",
  "colourpicker",
  "jsonlite"
))
```

Optional export packages:

```r
install.packages(c(
  "officer",
  "rvg",
  "gganimate",
  "gifski"
))
```

## Running The App

From the project folder:

```r
shiny::runApp(".")
```

Or run:

```powershell
Rscript run_app.R
```

The helper script also supports a fixed host and port:

```powershell
$env:CFU_APP_HOST = "127.0.0.1"
$env:CFU_APP_PORT = "4267"
Rscript run_app.R
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
saying so.

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
- `run_app.R`: app launcher.
- `dummy_cfu_example.csv`: synthetic example/template data.
- `outputs/`: local validation outputs and screenshots.

## License

This project is released under the MIT License.

## Citation And DOI

Citation metadata is provided in `CITATION.cff`. Zenodo metadata is provided in `.zenodo.json`.

After the repository is public on GitHub, connect it to Zenodo and create a GitHub release. Zenodo will archive that release and mint a DOI for it.

The initial release metadata lists Michael Baffour Awuah as maintainer. Update the affiliation field in `CITATION.cff` and `.zenodo.json` later if a formal institutional affiliation should be displayed.
