# CFU Plot Studio: Publication-Ready CFU Figures Without Spreadsheet Gymnastics

![CFU Plot Studio hero graphic](docs/assets/cfu-plot-studio-hero.svg)

Colony forming unit assays are beautifully direct at the bench: count colonies, calculate CFU, compare conditions. The figure-making side is often less beautiful. A single experiment can include several strains or vectors, multiple treatments, multiple timepoints, replicate plates, log-scaled counts, statistics, and journal-specific figure sizing. It is easy for a simple CFU assay to turn into a tangle of spreadsheet reshaping, repeated graph settings, copied statistics, and last-minute export fixes.

**CFU Plot Studio** is an R Shiny app built to make that workflow cleaner, more reproducible, and easier to share.

## The Short Version

CFU Plot Studio takes replicate-level CFU data and turns it into publication-focused bar plots, downloadable statistics, QC tables, and reproducible figure handoff files.

It is built for labs that want:

- A point-and-click interface for CFU plotting.
- Replicate-level statistics on `log10(CFU)`.
- SD, SEM, 95% CI, IQR, or min-max variation display.
- Visible replicate points.
- Reproducible figure dimensions and export settings.
- Editable SVG, PDF, PNG, PowerPoint, GIF, and reveal-slide exports.
- A saved preset, manifest, and R script that can recreate the figure later.

## The Workflow

![CFU Plot Studio workflow](docs/assets/cfu-workflow.svg)

1. Upload a normal CSV file.
2. Map the sample, treatment, timepoint, replicate, and CFU columns.
3. Check the QC tab for replicate structure and possible data issues.
4. Customize the plot, axis limits, ticks, colors, fonts, legend, and figure size.
5. Choose the statistics and variation display.
6. Export the figure, summary tables, statistics, preset, manifest, and reproducible R script.

## Why This Exists

For publication figures, a CFU plot usually needs more than a quick bar chart. It needs:

- Replicate-level input, not only precomputed averages.
- Clear variation display, such as SD, SEM, 95% CI, IQR, or min-max range.
- Visible replicate points for transparency.
- Statistics on an appropriate scale, usually `log10(CFU)`.
- Flexible comparisons such as strain versus strain, vector versus vector, timepoint changes, or treatment versus control.
- Exact figure sizing so plots can be reproduced across projects.
- Export formats that work for manuscripts, talks, posters, and final editing.

Doing this manually is possible, but it is also easy to lose track of what was tested, which axis limits were used, or whether the exported figure still matches the final statistics.

## What The App Does

CFU Plot Studio starts from a normal CSV file. The data can have custom column names because the app lets you map the important fields:

- Sample, vector, strain, or individual
- Treatment, dose, condition, or concentration
- Timepoint
- Replicate
- CFU count

Once the file is mapped, the app creates cleaned data, summary tables, QC checks, statistics, and publication-style plots.

## Publication-Focused Plotting

The plotting controls are built around manuscript needs. Users can set exact figure dimensions in inches, DPI, y-axis boundaries, major and minor tick spacing, plot theme, font sizes, legend position, and export format.

The app supports:

- `log10(CFU)` plots
- Raw CFU values on a log axis
- SD, SEM, 95% CI, IQR, or min-max intervals
- Capped error bars, uncapped whiskers, mean point plus whiskers, crossbar intervals, or replicate-points-only display
- Custom sample and timepoint colors
- Optional boxed plot panels
- Major and minor y-axis ticks and guide lines
- Inside or outside legend placement
- Editable units for treatment and time labels
- Single-column, double-column, and square figure size presets

The treatment axis is generic. The same field can represent drug dose, infection condition, medium condition, temperature, growth condition, induction condition, or another lab-specific variable.

## Statistics That Stay With The Figure

The app performs statistics on replicate-level `log10(CFU)` values. It can compare:

- Samples or vectors within each treatment and timepoint
- Timepoints within each sample and treatment
- Each treatment versus a selected control
- All treatment pairs within each sample and timepoint

Welch t-tests are the default, with Student t-tests and `emmeans` available when needed. Multiple-comparison adjustment can be applied globally or within each panel or group, and results can be shown as stars or adjusted p/q values.

The statistics table is downloadable, so the figure and its underlying tests can be archived together.

## Reproducible Figure Handoff

The final figure should not be a dead image. CFU Plot Studio can export:

- A plot preset JSON file
- An analysis manifest JSON file
- A reproducible R script that recreates the visible plot
- Cleaned data, summary data, QC tables, statistics, and ANOVA output

This makes it easier to move from interactive exploration to a project folder that can be shared, versioned, uploaded to GitHub, or archived with a release.

## Exporting For Real Use

The app exports:

- PNG for high-resolution raster figures
- PDF and SVG for vector editing
- PowerPoint for slide workflows
- Editable PowerPoint vector art when `rvg` is installed
- Animated GIFs and reveal-slide PowerPoints for presentations
- Cleaned data, summaries, QC tables, and statistics

This makes the app useful beyond a single manuscript panel. It can support lab meetings, talks, supplementary figures, release records, and reproducible figure handoff.

## Bug Reports And Contact

The best way to report a bug is through the GitHub repository:

- **Bug reports:** open a GitHub Issue with a short description, your operating system, R version, and a small example CSV if possible.
- **Feature requests:** open a GitHub Issue and label it as an enhancement.
- **Questions or lab-use feedback:** use GitHub Discussions if enabled, or contact the maintainer.
- **Repository:** <https://github.com/mbaffour/cfu-plot-studio>
- **Maintainer:** Michael Baffour Awuah, via GitHub Issues or the GitHub profile connected to the repository.

Please do not include private or unpublished experimental data in a public issue. If a bug requires data to reproduce, make a small synthetic example that has the same structure.

## Citation And DOI

The project can be made citable by archiving a GitHub release with Zenodo. Zenodo will mint a DOI for the archived release, and future releases can receive version-specific DOIs. A `CITATION.cff` file and `.zenodo.json` metadata file are included so the repository is ready for that workflow.

## What Comes Next

Useful future additions could include:

- Saved named presets inside the app.
- Import/export of complete project sessions.
- More plot types, such as dot plots, box plots, and raincloud-style plots.
- Built-in example datasets for common CFU experiment designs.
- A hosted Shiny deployment.

CFU Plot Studio is meant to be a practical lab utility: polished enough for publication figures, flexible enough for different experiment designs, and transparent enough to keep the data, statistics, and final graph connected.
