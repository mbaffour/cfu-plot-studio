# CFU Plot Studio v0.1.0

Initial public release of CFU Plot Studio.

## Highlights

- R Shiny app for replicate-level CFU plotting.
- Neutral synthetic example dataset.
- Publication-focused bar plots with customizable fonts, colors, axis limits, major/minor ticks, legend placement, plot boxes, and export dimensions.
- Variation summaries: SD, SEM, 95% CI, IQR, and min-max range.
- Variation display styles: capped error bars, uncapped whiskers, mean point plus whiskers, crossbar intervals, or replicate points only.
- Replicate-level statistics on `log10(CFU)`.
- Supported comparisons: sample/vector, timepoint, treatment versus control, and all treatment pairs.
- Multiple-comparison correction options.
- Figure QA checklist.
- Exports for PNG, PDF, SVG, PowerPoint, animated GIF, reveal-slide PowerPoint, cleaned data, summary tables, QC tables, statistics, and ANOVA.
- Reproducibility exports: plot preset JSON, analysis manifest JSON, and reproducible R script.
- GitHub Pages landing page and blog-style project post.
- Citation and Zenodo metadata files.

## Validation

- App parses with R 4.3.2.
- Neutral demo data loads and plots.
- Public release folder scanned for private experiment terms.
- GitHub Pages builds from `docs/`.
