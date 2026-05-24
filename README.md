# CFU Plot Studio

CFU Plot Studio is an R Shiny app for turning replicate-level colony forming unit data into publication-focused bar plots, summary tables, QC checks, and statistics.

The app is designed for microbiology lab workflows where data are usually collected as rows of replicate measurements across samples, treatments, timepoints, and CFU counts.

## What It Does

- Imports replicate-level CSV files without requiring a fixed column order.
- Maps sample/vector, treatment/dose/condition, timepoint, replicate, and CFU columns inside the app.
- Plots CFU summaries as bar plots with SD, SEM, 95% CI, IQR, or min-max variation intervals.
- Shows individual replicate points on top of bars.
- Runs replicate-level statistics on `log10(CFU)`.
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

## Publication Figure Controls

The app includes controls for:

- Exact export width, height, and DPI.
- Reproducible size presets for single-column, double-column, and square figures.
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

## Statistics

The default statistics use Welch t-tests on `log10(CFU)` values. The app also supports Student t-tests and model-based marginal means through `emmeans`.

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
