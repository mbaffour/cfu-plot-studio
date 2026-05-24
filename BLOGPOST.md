# CFU Plot Studio: Publication-Ready CFU Figures Without Spreadsheet Gymnastics

![CFU Plot Studio hero graphic](docs/assets/cfu-plot-studio-hero.svg)

Colony forming unit assays are beautifully direct at the bench: count colonies, calculate CFU, compare conditions. The figure-making side is often less beautiful. A single experiment can include several strains or vectors, multiple treatments, multiple timepoints, replicate plates, log-scaled counts, statistics, and journal-specific figure sizing. It is easy for a simple CFU assay to turn into a tangle of spreadsheet reshaping, repeated graph settings, copied statistics, and last-minute export fixes.

**CFU Plot Studio** is an R Shiny app built to make that workflow cleaner, more reproducible, and easier to share.

Repository: <https://github.com/mbaffour/cfu-plot-studio>  
Report a bug: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=bug_report.md>  
Request a feature: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=feature_request.md>

## What CFU Plot Studio Does

CFU Plot Studio takes replicate-level CFU data and turns it into publication-focused bar plots, downloadable statistics, QC tables, and reproducible figure handoff files.

It is built for labs that want:

- A point-and-click interface for CFU plotting.
- Replicate-level data input rather than precomputed averages only.
- Statistics on `log10(CFU)`.
- SD, SEM, 95% CI, IQR, or min-max variation display.
- Visible replicate points.
- Reproducible figure dimensions and export settings.
- Editable SVG, PDF, PNG, PowerPoint, GIF, and reveal-slide exports.
- A saved preset, analysis manifest, and R script that can recreate the figure later.

## Workflow Overview

![CFU Plot Studio workflow](docs/assets/cfu-workflow.svg)

1. Upload a normal CSV file.
2. Map the sample, treatment, timepoint, replicate, and CFU columns.
3. Check the QC tab for replicate structure and possible data issues.
4. Customize the plot, axis limits, ticks, colors, fonts, legend, and figure size.
5. Choose the statistics and variation display.
6. Export the figure, summary tables, statistics, preset, manifest, and reproducible R script.

## Input Data Format

The app expects one row per replicate measurement. Column names do not have to match exactly because the app has column-mapping controls.

Required information:

| Field | Example |
| --- | --- |
| Sample, strain, vector, or group | `Control strain`, `Test strain` |
| Treatment, dose, condition, or concentration | `Baseline`, `Treatment A`, `Treatment B` |
| Timepoint | `Early`, `Late`, `0`, `120` |
| Replicate | `1`, `2`, `3` |
| CFU count | `4300000` |

The repository includes a neutral synthetic file, `dummy_cfu_example.csv`, that can be used as a template.

## How To Use The App

### 1. Run The App

Install the core R packages:

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

Optional packages for PowerPoint and animation exports:

```r
install.packages(c(
  "officer",
  "rvg",
  "gganimate",
  "gifski"
))
```

Run from the project folder:

```r
shiny::runApp(".")
```

or:

```powershell
Rscript run_app.R
```

### 2. Upload Or Use The Template Data

Use the file upload control for your CSV. If you are just testing the tool, load the built-in dummy example data. The dummy data are intentionally neutral and synthetic.

### 3. Map Columns

Choose which columns represent:

- sample/vector/group
- treatment/dose/condition
- timepoint
- replicate
- CFU count

This means the app can work with different lab spreadsheet styles without requiring a fixed column naming convention.

### 4. Choose A Plot Mode

The app currently supports:

- **Combined samples, faceted by time:** good for comparing groups across treatments at each timepoint.
- **One sample, both timepoints:** good for looking at a single group across time.
- **One sample, one timepoint:** good for focused treatment-response plots.

### 5. Choose Variation Display

Variation can be summarized as:

- **SD:** common for showing spread around the mean.
- **SEM:** useful for uncertainty around the mean, but often less transparent for small replicate counts.
- **95% CI:** confidence interval around the mean.
- **IQR:** interquartile range, useful for showing replicate spread without relying on normality.
- **Range (min-max):** shows the full visible replicate range.

Variation can be drawn as:

- capped error bars
- uncapped whiskers
- mean point plus whiskers
- crossbar interval
- replicate points only

For CFU assays with small replicate numbers, a good default is to keep replicate points visible and use SD or min-max range depending on the figure's purpose.

### 6. Configure Statistics

The app performs statistics on replicate-level `log10(CFU)` values. It can compare:

- samples or vectors within each treatment and timepoint
- timepoints within each sample and treatment
- each treatment versus a selected control
- all treatment pairs within each sample and timepoint

Available methods:

- Welch t-test on `log10(CFU)` values
- Student t-test on `log10(CFU)` values
- linear model plus `emmeans`

Multiple-comparison correction options:

- BH
- Holm
- Bonferroni
- none

Statistic labels can be shown as stars or exact adjusted p/q values.

### 7. Make The Figure Publication-Ready

Plot controls include:

- exact export width, height, and DPI
- single-column, double-column, and square size presets
- y-axis boundaries
- major and minor y-axis tick spacing
- major and minor y-axis guide lines
- plot box and axis line widths
- bar width, dodge width, point size, point alpha, and jitter
- axis, grid, outline, sample, timepoint, and statistic-label colors
- title, subtitle, x-label, legend title, and font sizes
- inside or outside legend placement
- editable treatment and time unit suffixes

### 8. Use Figure QA

The Figure QA tab checks for common publication problems:

- low replicate counts
- hidden replicate points
- low export DPI
- very small figure dimensions
- small fonts
- crowded x-axis labels
- statistics labels that may clip
- inside legends that may cover data

It is not a replacement for visual inspection, but it catches common problems before export.

### 9. Export The Figure And Handoff Files

The app can export:

- PNG
- PDF
- SVG
- PowerPoint
- editable PowerPoint vector art when `rvg` is installed
- animated GIF
- reveal-slide PowerPoint
- cleaned data
- summary tables
- QC tables
- statistics tables
- ANOVA tables
- plot preset JSON
- analysis manifest JSON
- reproducible R script

The preset, manifest, and R script are especially useful for reproducibility. They help preserve not only the image, but also the settings and data used to make it.

## Technical Components

CFU Plot Studio is built as a single-file R Shiny app with a small launcher script.

Main components:

- **Shiny:** interactive app framework.
- **ggplot2:** plotting engine.
- **dplyr:** data cleaning, grouping, summaries, QC, and reactive transformations.
- **readr:** CSV import/export.
- **emmeans:** model-based marginal means and contrasts.
- **broom:** tidy model/statistics output.
- **DT:** interactive tables.
- **colourpicker:** color controls in the UI.
- **jsonlite:** plot presets, analysis manifests, and reproducibility exports.
- **officer:** PowerPoint export.
- **rvg:** editable vector graphics in PowerPoint.
- **gganimate and gifski:** animated GIF export.

Repository components:

- `app.R`: main Shiny application.
- `run_app.R`: launcher script.
- `dummy_cfu_example.csv`: neutral synthetic template data.
- `README.md`: installation and usage guide.
- `BLOGPOST.md`: project post and user guide.
- `CITATION.cff`: citation metadata.
- `.zenodo.json`: Zenodo metadata.
- `LICENSE`: MIT License.
- `docs/`: GitHub Pages landing page and graphics.
- `.github/ISSUE_TEMPLATE/`: bug and feature request forms.

## Bug Reports And Contact

The best way to report a bug is through GitHub Issues:

- Bug report: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=bug_report.md>
- Feature request: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=feature_request.md>
- All issues: <https://github.com/mbaffour/cfu-plot-studio/issues>

When reporting a bug, include:

- operating system
- R version
- browser
- what you clicked or changed
- error message or screenshot
- a small synthetic CSV if data are needed to reproduce the issue

Please do not include private or unpublished experimental data in a public issue. If a bug requires data to reproduce, make a small synthetic example with the same structure.

Maintainer: Michael Baffour Awuah, through the GitHub repository.

## Citation And DOI

The project is prepared for Zenodo archival. The repository includes:

- `CITATION.cff`
- `.zenodo.json`
- `LICENSE`

When a GitHub release is archived by Zenodo, Zenodo mints a DOI for that software release. Future releases can receive version-specific DOIs.

## What Comes Next

Useful future additions could include:

- saved named presets inside the app
- complete project-session export/import
- dot plots, box plots, and raincloud-style plots
- hosted Shiny deployment
- more built-in synthetic examples for common CFU experimental designs

CFU Plot Studio is meant to be a practical lab utility: polished enough for publication figures, flexible enough for different experiment designs, and transparent enough to keep the data, statistics, and final graph connected.
