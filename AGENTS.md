# Repository Guidelines

## Project Structure & Module Organization
The workshop sources live in `R/` (core helpers like `functions.R`, `mh.R`, `hmc.R`) and are orchestrated by `_targets.R`. Stan models used in the pipeline reside in `stan/`. Quarto slide decks (`main.qmd`, `docs/*.qmd`) and published HTML live in the project root and `docs/`. Generated assets and GIFs should land in `images/` or `assets/`; `_targets/` stores pipeline metadata and must be treated as cache. Environment scaffolding is handled via `renv.lock`, the `renv/` directory, and container scripts in `scripts/`.

## Build, Test, and Development Commands
Run `R -q -e "renv::restore()"` on first setup to match package versions. Use `Rscript R/run_script.R 1` for a local single-core build of the `targets` pipeline. `./run.sh` provides an interactive launcher for local, Apptainer, or Singularity runs; choose option 2 or 4 for HPC contexts. Call `R -q -e "targets::tar_meta()"` after a run to confirm target status, and `R -q -e "targets::tar_destroy()"` to clear cache when pipelines change significantly.

## Coding Style & Naming Conventions
Follow tidyverse style: two-space indentation, spaces around `=` in arguments, and pipe-friendly line breaks. Use snake_case for R objects (`generate_dummy_simple`), and keep Stan parameter names lowercase with underscores. Prefer explicit `library()` calls inside `_targets.R`, and document side effects with inline comments when logic is non-obvious. Run `R -q -e "styler::style_dir('R')"` before committing to keep formatting consistent.

## Testing Guidelines
There is no standalone test suite; reliability comes from the `targets` workflow. Add new targets for derived artefacts and ensure they return tibbles or files consumed downstream. Use `R -q -e "targets::tar_validate()"` to catch missing dependencies and `R -q -e "targets::tar_make_clustermq(workers = 4)"` to mimic multi-core execution before pushing. Regenerate GIFs and HTML locally and verify outputs under `images/` or `docs/`.

## Commit & Pull Request Guidelines
Commit messages in history are short present-tense statements (`update docker`, `add scripts`); follow that style and keep scope limited per commit. PRs should describe the change set, note any new targets or Stan models, link workshop issues, and include screenshots or rendered artefact paths when updating slides or animation outputs. Confirm `renv.lock` updates are intentional; otherwise avoid noise by using the existing lockfile.
