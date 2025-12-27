# Repository Guidelines

## Project Structure & Module Organization
R scripts live in `scripts/` and are grouped by workflow: `load_machines_subset.R` prepares the rrcov `machines` data, while analysis variants (`pca_original_scale.R`, `std_pca.R`, `robust_pca.R`, etc.) each encapsulate a full experiment. Generated figures and diagnostics are stored under `plots/`, split into subdirectories per experiment (for example `plots/plots_pca_robust_mcd`). PDF exports and reports sit in the root (`Report_MEIA_Project1.pdf`, `scripts.pdf`) so keep derived artifacts out of version control unless they summarize new findings.

## Build, Test, and Development Commands
Run analyses with `Rscript` from the repo root:
- `Rscript scripts/exploratory.R` — initial EDA tables and sanity checks.
- `Rscript scripts/pca_original_scale.R` — classical PCA on original scale, updates `plots/plots_pca_original`.
- `Rscript scripts/robust_pca.R` — robust MCD PCA with injected outlier, writes PNGs to `plots/plots_pca_robust_mcd` and prints variance tables.
- `Rscript scripts/std_pca.R` — standardized PCA, mirrors the structure needed for additions.
Use `R -e "devtools::document()"` only if you introduce reusable packages or helpers; otherwise plain scripts keep dependencies minimal.

## Coding Style & Naming Conventions
Adopt tidyverse-friendly R style: two-space indents, `snake_case` functions (`perform_robust_pca`), and explicit argument names for readability. Keep side effects (plots, logs) in dedicated helper functions so each script can be sourced safely. Prefer `library()` calls near the top and guard them with `requireNamespace` checks as seen in `load_machines_subset.R`. Store assets via `file.path()` helpers to avoid OS-specific separators.

## Testing Guidelines
No formal unit tests exist, so lean on reproducible script runs. Before pushing, rerun the focal script plus `Rscript scripts/load_machines_subset.R` to confirm the data slice resolves and prints the expected row count. Validate new analyses by diffing generated PNGs (e.g., `git diff --stat plots/plots_pca_robust_mcd`) and capturing console summaries inside `scripts/tests-notes.md` if manual QA is needed. When adding automated checks, keep files under `scripts/tests/` and name them `test_<feature>.R`.

## Commit & Pull Request Guidelines
Recent history uses short, present-tense subjects (e.g., `improve robust pca`, `update outlier plot`). Follow that format, limit to 72 characters, and reference issues with `#id` when relevant. Pull requests should detail the motivation, outline datasets touched, list new commands to run, and attach key plots or console snippets so reviewers can confirm numerics without rerunning everything. Mention any CRAN packages that now need installation so deployment scripts stay up to date.
