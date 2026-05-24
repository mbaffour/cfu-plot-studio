# CFU Plot Studio: a publication-focused Shiny app for CFU graphs, statistics, and reproducible figure export

![CFU Plot Studio hero graphic](docs/assets/cfu-plot-studio-hero.svg)

Colony forming unit assays are simple at the bench, but the figure workflow can become surprisingly messy. A single experiment may have multiple strains, plasmids, vectors, treatments, timepoints, replicate plates, log-scaled counts, and several versions of the same graph for talks, manuscripts, lab meetings, and supplements.

**CFU Plot Studio** is an R Shiny app built to make that process smoother. It takes replicate-level CFU data, lets you map your columns, makes publication-ready bar plots, runs statistics on `log10(CFU)`, checks common figure-quality problems, and exports the figure plus the data products needed to reproduce it later.

Repository: <https://github.com/mbaffour/cfu-plot-studio>  
Release: <https://github.com/mbaffour/cfu-plot-studio/releases/tag/v0.1.0>  
Open a bug report: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=bug_report.md>  
Request a feature: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=feature_request.md>  
All issues: <https://github.com/mbaffour/cfu-plot-studio/issues>

## Why this tool exists

CFU data are usually collected as replicate measurements, but figures are often assembled after several manual steps:

- copying counts into a spreadsheet
- calculating means and standard deviations
- reshaping tables for graphing
- switching between raw counts and log scales
- adding significance labels by hand
- exporting a figure, then realizing the axis, legend, or font size is not right
- repeating those settings for a second figure or a later experiment

That workflow is fragile. It is easy to lose track of which data, statistics, and export settings produced the final figure. CFU Plot Studio keeps those pieces connected.

## What CFU Plot Studio does

![CFU Plot Studio workflow](docs/assets/cfu-workflow.svg)

The app is designed for microbiology and infection biology workflows where CFU counts are measured across groups, conditions, and timepoints.

It can:

- import replicate-level CSV data
- use neutral built-in dummy data as a safe template
- map user-defined columns for sample, treatment, timepoint, replicate, and CFU count
- make bar plots with replicate points and selectable variation summaries
- compare groups with statistics on `log10(CFU)`
- display significance as stars or adjusted p/q values
- hide statistics completely for clean figure versions
- generate single-timepoint plots or combined multi-timepoint plots
- make plots for empty-vector style controls, plasmid-containing vectors, strains, treatments, or any other group names
- customize axis limits, major ticks, minor ticks, y-axis tick marks, grid lines, plot boxes, labels, legends, colors, and figure dimensions
- export figures as PNG, PDF, SVG, PowerPoint, animated GIF, and reveal-slide PowerPoint
- export cleaned data, summary tables, QC tables, statistics tables, ANOVA tables, plot presets, analysis manifests, and reproducible R scripts

## App component map

![CFU Plot Studio component map](docs/assets/cfu-component-map.svg)

The app is organized around a practical lab workflow:

1. **Data input:** upload a CSV or use the dummy template.
2. **Column mapping:** tell the app which columns mean group, condition, timepoint, replicate, and CFU.
3. **Quality control:** inspect replicate counts and possible data issues.
4. **Statistics:** choose the comparison and correction method.
5. **Plot styling:** adjust figure geometry, axes, colors, legend, labels, and variation display.
6. **Export:** download the graph, tables, preset, manifest, or reproducible R script.

## Data format

The app expects one row per replicate measurement. The column names do not need to be fixed because the app has column-mapping controls.

Required information:

| Field | Example values |
| --- | --- |
| Sample, strain, vector, plasmid, or group | `Control strain`, `Test strain`, `Empty vector`, `Plasmid vector` |
| Treatment, dose, condition, or concentration | `Baseline`, `Treatment A`, `Treatment B`, `0`, `10` |
| Timepoint | `Early`, `Late`, `0 h`, `24 h` |
| Replicate | `1`, `2`, `3` |
| CFU count | `4300000`, `2.1e6`, `95000` |

Example CSV structure:

```csv
Group,Treatment,Timepoint,Replicate,CFU
Control strain,Baseline,Early,1,4300000
Control strain,Baseline,Early,2,5100000
Control strain,Baseline,Early,3,4700000
Test strain,Treatment A,Late,1,2300000
Test strain,Treatment A,Late,2,2800000
Test strain,Treatment A,Late,3,2600000
```

The included `dummy_cfu_example.csv` is synthetic and neutral. It is meant to be safe to share publicly and useful as a formatting template.

## How to run it

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

Optional packages enable PowerPoint and animation exports:

```r
install.packages(c(
  "officer",
  "rvg",
  "gganimate",
  "gifski"
))
```

Run the app from the project folder:

```r
shiny::runApp(".")
```

Or use the helper script:

```powershell
Rscript run_app.R
```

For a fixed local address:

```powershell
$env:CFU_APP_HOST = "127.0.0.1"
$env:CFU_APP_PORT = "4267"
Rscript run_app.R
```

Then open:

```text
http://127.0.0.1:4267/
```

## Typical workflow

### 1. Upload or load data

Start with your CSV file or the dummy template. The app keeps the uploaded data at replicate level so the graph and statistics are based on the same underlying measurements.

### 2. Map the columns

Use the mapping controls to select:

- group/sample/vector/plasmid column
- treatment/dose/condition column
- timepoint column
- replicate column
- CFU count column

This makes the app flexible for different spreadsheet styles. A lab can call the same concept `Group`, `Strain`, `Vector`, `Condition`, or `Construct`, and still use the tool.

### 3. Choose the graph scope

The plotting modes are useful for different figure panels:

- **Combined samples, faceted by time:** compare multiple groups across treatments and timepoints.
- **One sample, both timepoints:** focus on a single strain, vector, or construct across time.
- **One sample, one timepoint:** create a clean single-panel plot for a selected timepoint.

This supports both broad overview figures and focused manuscript panels.

### 4. Choose how variation is shown

The app can summarize variation as:

- **SD:** spread of replicate values around the mean
- **SEM:** uncertainty around the mean
- **95% CI:** confidence interval around the mean
- **IQR:** interquartile range
- **Range:** minimum to maximum replicate value

The visual display can be:

- capped error bars
- uncapped line ranges
- mean point plus whiskers
- crossbar intervals
- replicate points only

For small CFU experiments, showing individual replicate points is strongly recommended. It lets readers see whether a result is supported by consistent replicate behavior or by one extreme value.

### 5. Run statistics

Statistics are performed on replicate-level `log10(CFU)` values. This is usually more appropriate than testing raw CFU counts because CFU values often span orders of magnitude.

Available comparisons include:

- samples or vectors within each treatment and timepoint
- timepoints within each sample and treatment
- each treatment versus a selected control
- all treatment pairs within each sample and timepoint
- no statistics, for clean figure exports

Available methods include:

- Welch t-test on `log10(CFU)`
- Student t-test on `log10(CFU)`
- linear model plus `emmeans`

Multiple-comparison correction options include:

- BH
- Holm
- Bonferroni
- none

Statistic labels can be shown as:

- stars
- adjusted p/q values
- hidden labels

The statistics table can be exported separately so the numbers behind the figure are preserved.

### 6. Make the figure publication-ready

The plot controls are meant to reduce the amount of manual editing needed after export.

Figure geometry:

- exact width and height in inches
- DPI control
- single-column, double-column, and square presets
- horizontal or vertical bar orientation
- bar width and dodge width

Axes:

- `log10(CFU)` display
- raw CFU on a log axis
- manual y-axis minimum and maximum
- automatic y-axis reset
- major tick spacing
- minor tick spacing
- major grid lines
- minor grid lines
- y-axis tick marks
- minor y-axis tick marks
- plot box on/off
- axis and box line width

Labels and units:

- plot title
- subtitle
- x-axis label
- y-axis label behavior
- legend title
- treatment unit suffix
- time unit suffix
- custom text sizes

Appearance:

- sample colors
- timepoint colors
- axis color
- grid color
- bar outline color
- statistic-label color
- point size, alpha, and jitter
- legend position at top, right, bottom, left, inside, or hidden

### 7. Use Figure QA before exporting

The Figure QA tab checks common problems:

- low replicate counts
- hidden replicate points
- low DPI
- small figure dimensions
- small font sizes
- crowded x-axis labels
- statistics labels that may be clipped
- inside legends that may cover data

The QA table is not a replacement for visually inspecting the plot, but it helps catch common figure problems before submission.

### 8. Export and archive the figure

The app exports:

- PNG
- PDF
- SVG
- PowerPoint
- editable PowerPoint vector art when `rvg` is installed
- animated GIF
- reveal-slide PowerPoint
- cleaned CSV
- summary statistics CSV
- QC CSV
- Figure QA CSV
- statistical results CSV
- ANOVA CSV
- plot preset JSON
- analysis manifest JSON
- reproducible R script

For manuscripts, a good habit is to keep four things together:

- the final figure
- the exported statistics table
- the plot preset JSON
- the analysis manifest or reproducible R script

That way, future you can reconstruct what was plotted and how it was styled.

## Technical components

CFU Plot Studio is currently implemented as a single-file R Shiny app plus a small launcher script.

Core files:

| File | Purpose |
| --- | --- |
| `app.R` | Main Shiny application |
| `run_app.R` | Local launcher with optional host and port settings |
| `dummy_cfu_example.csv` | Synthetic template data |
| `README.md` | Installation and usage guide |
| `BLOGPOST.md` | Launch article and user guide |
| `CITATION.cff` | Citation metadata |
| `.zenodo.json` | Zenodo metadata |
| `LICENSE` | MIT License |
| `docs/` | GitHub Pages site and graphics |
| `.github/ISSUE_TEMPLATE/` | Bug and feature request templates |

R packages used:

| Package | Role |
| --- | --- |
| `shiny` | Interactive web application framework |
| `ggplot2` | Plot construction and publication figure rendering |
| `dplyr` | Data cleaning, grouping, summaries, and QC |
| `readr` | CSV import and export |
| `emmeans` | Model-based contrasts and marginal means |
| `broom` | Tidy statistical output |
| `DT` | Interactive tables |
| `colourpicker` | Color controls in the UI |
| `jsonlite` | Presets, manifests, and JSON export |
| `officer` | PowerPoint export |
| `rvg` | Editable vector graphics in PowerPoint |
| `gganimate` | Bar reveal animation |
| `gifski` | GIF rendering |

## Reproducibility features

The app is not only a graph maker. It also records the figure context.

The preset JSON stores the visual settings. The analysis manifest records the mapped columns, selected options, active plot mode, statistics choices, and export settings. The reproducible R script gives a starting point for recreating the graph outside the Shiny interface.

These exports are useful for:

- manuscript revision
- lab notebooks
- project handoff
- troubleshooting
- comparing figure versions across projects

## Bug reports and contact

Please use GitHub Issues for bug reports and feature requests:

- Bug report: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=bug_report.md>
- Feature request: <https://github.com/mbaffour/cfu-plot-studio/issues/new?template=feature_request.md>
- All issues: <https://github.com/mbaffour/cfu-plot-studio/issues>

Helpful bug reports include:

- operating system
- R version
- browser
- app version or GitHub release tag
- what you clicked or changed
- the exact error message
- screenshot, if useful
- a small synthetic CSV that reproduces the problem

Please do not post private or unpublished experimental data in a public issue. If data are needed to reproduce a bug, make a small dummy file with the same column structure.

Maintainer: Michael Baffour Awuah, through the GitHub repository.

## Citation and DOI

The project is prepared for Zenodo archival. The repository includes:

- `CITATION.cff`
- `.zenodo.json`
- `LICENSE`

After the GitHub repository is enabled in Zenodo and a GitHub release is archived, Zenodo can mint a DOI for the software release. The current release is:

```text
CFU Plot Studio v0.1.0
https://github.com/mbaffour/cfu-plot-studio/releases/tag/v0.1.0
```

Once Zenodo finishes archiving the release, the DOI can be added to this section and to the README.

## Suggested citation text before DOI assignment

```text
Awuah, M. B. CFU Plot Studio: an R Shiny app for publication-ready CFU plots, replicate-level statistics, and reproducible figure export. Version 0.1.0. GitHub: https://github.com/mbaffour/cfu-plot-studio
```

## What comes next

Future directions could include:

- hosted Shiny deployment
- additional built-in synthetic datasets
- box plot, dot plot, and raincloud-style figure modes
- project-session export/import
- named in-app presets
- more statistical model options for complex experimental designs
- automated visual regression checks for plot rendering

CFU Plot Studio is meant to be a practical lab utility: polished enough for publication figures, flexible enough for real experimental layouts, and transparent enough to keep the data, statistics, and final graph connected.
