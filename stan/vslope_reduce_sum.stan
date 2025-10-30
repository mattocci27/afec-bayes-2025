functions {
  // Partial log-likelihood over a slice of observation indices
  // Note: do NOT end name with _lpdf/_lpmf to avoid special parsing.
  real vslope_partial_sum(array[] int n_slice,
                          int start, int end,
                          vector y,
                          matrix x,
                          array[] int jj,
                          matrix beta,
                          real sigma) {
    real lp = 0;
    int S = size(n_slice);
    for (s in 1:S) {
      int n = n_slice[s];
      real mu_n = x[n, ] * beta[, jj[n]];  // row_vector x[n, ] * vector
      lp += normal_lpdf(y[n] | mu_n, sigma);
    }
    return lp;
  }
}

data {
  int<lower=0> N;                          // num observations
  int<lower=1> K;                          // num individual predictors
  int<lower=1> J;                          // num groups
  array[N] int<lower=1, upper=J> jj;       // group index per obs
  int<lower=1> L;                          // num group-level predictors
  matrix[N, K] x;                          // individual predictors
  matrix[L, J] u;                          // group predictors
  vector[N] y;                              // outcomes
}

transformed data {
  // Indices 1..N used for slicing with reduce_sum
  array[N] int n_obs;
  for (n in 1:N) n_obs[n] = n;
}

parameters {
  matrix[K, L] gamma;                      // coefficients across groups
  real<lower=0> sigma;                     // observation noise scale
  vector<lower=0>[K] tau;                  // prior scale per predictor
  matrix[K, J] z;                          // standardized group effects
  cholesky_factor_corr[K] L_Omega;         // Cholesky of corr among K effects
}

transformed parameters {
  matrix[K, J] beta;                        // group-specific coefficients
  beta = gamma * u + diag_pre_multiply(tau, L_Omega) * z;
}

model {
  // Priors
  to_vector(z) ~ std_normal();
  tau ~ normal(0, 2.5);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(gamma) ~ normal(0, 2.5);

  // Likelihood via within-chain parallelization
  // Adjust grainsize to tune overhead vs. work per task.
  target += reduce_sum(vslope_partial_sum, n_obs, 1,
                       y, x, jj, beta, sigma);
}
