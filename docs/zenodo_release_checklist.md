# Zenodo DOI Release Checklist

Use this checklist before creating the first GitHub release for CFU Plot Studio.

## One-Time Setup

1. Log in to Zenodo.
2. Connect Zenodo to GitHub.
3. In Zenodo's GitHub repository list, enable `mbaffour/cfu-plot-studio`.
4. Confirm the repository contains:
   - `CITATION.cff`
   - `.zenodo.json`
   - `LICENSE`
   - `README.md`

## Create The First DOI-Minting Release

After Zenodo is enabled for the repository:

1. Create a GitHub release named `v0.1.0`.
2. Use the title `CFU Plot Studio v0.1.0`.
3. Paste the release notes from the local release summary or GitHub changelog.
4. Publish the release.
5. Wait for Zenodo to archive the release.
6. Copy the minted DOI into the README and future blog/project pages.

## Notes

- Zenodo's GitHub integration mints the DOI when it archives a GitHub release.
- If a DOI is needed before GitHub release archival, use a manual Zenodo upload instead.
- Do not include private data files or generated local outputs in the release archive.
