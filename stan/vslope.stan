data {
  int<lower=0> N;              // num individuals
  int<lower=1> K;              // num ind predictors
  int<lower=1> J;              // num groups
  array[N] int<lower=1, upper=J> jj;  // group for individual
  int<lower=1> L;              // number of group-level predictors
  matrix[N, K] x;              // individual predictors
  matrix[L, J] u;              // group predictors
  vector[N] y;                 // outcomes
}

parameters {
  matrix[K, L] gamma;          // coefficients across groups
  real<lower=0> sigma;         // prediction error scale
  vector<lower=0>[K] tau;      // prior scale
  matrix[K, J] z;
  cholesky_factor_corr[K] L_Omega;
}

transformed parameters {
  matrix[K, J] beta;
  beta = gamma * u + diag_pre_multiply(tau, L_Omega) * z;
}

model {
  to_vector(z) ~ std_normal();
  tau ~ cauchy(0, 2.5);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(gamma) ~ normal(0, 2.5);
  for (n in 1:N) {
    y[n] ~ normal(x[n, ] * beta[, jj[n]], sigma);
  }
}
