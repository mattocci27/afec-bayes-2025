[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

# Experimental Design and Statistics in R: AFEC 2025

Workshop materials for the AFEC program session on 2025-11-03 at XTBG.
The repository contains Quarto slide decks, guided exercises, and supporting Stan/R code for Bayesian modeling demonstrations.

- View the accompanying slides [here](https://forest-canopy.github.io/assets/afec-bayes-2025/main.html).
- View the accompanying exercise [here](https://forest-canopy.github.io/assets/afec-bayes-2025/exercise.html).

## Repository structure

- `R/` R helper scripts such as `functions.R`; `_targets.R` uses them to run the workflow, but we do not use them in the exercise.
- `stan/` Stan model files for the examples.
- `main.qmd`, `exercise.qmd`, `setup.Rmd` Quarto sources; the rendered HTML files live here or in `docs/`.
- `images/`, `assets/`, `figure/` saved plots, GIFs, and other media.
- `_targets/` cache created by the `targets` pipeline (rebuild with `targets::tar_make_*` if needed); not needed for the exercise.
- `renv.lock`, `renv/`, `scripts/` files for the R environment and container helpers.

## References

- [Gelman, A. et al. Bayesian Data Analysis, Third Edition. (Chapman & Hall/CRC, 2013)](http://www.stat.columbia.edu/~gelman/book/)

- [Stan User's Guide](https://mc-stan.org/docs/stan-users-guide/index.html)

- [Stan Functions Reference](https://mc-stan.org/docs/functions-reference/index.html)

- [AIcia Solid Project](https://youtu.be/mX_NpDD7wwg) (in Japanese and Math)

- [DanyangDai/brownbag_2022_10_11](https://github.com/DanyangDai/brownbag_2022_10_11)

- [emitanaka/talks](https://github.com/emitanaka/talks)

- Song, Xiaoyang, Masatoshi Katabuchi, Jonathan M. Chase, Daniel J. Johnson, Wenfu Zhang, Xiaobao Deng, Min Cao, and Jie Yang. 2024. “ Drought Tolerance and Species Abundance Mediate Dry Season Negative Density Dependence in a Tropical Forest.” Ecology 105(9): e4382. https://doi.org/10.1002/ecy.4382

- Nguyen, T.D., Katabuchi, M. Saturating allometric relationships reveal how wood density shapes global tree architecture. J. For. Res. 36, 107 (2025). https://doi.org/10.1007/s11676-025-01898-9
