data {
  int<lower=1> N;               // total number of observations
  int<lower=1> J;               // number of species
  array[N] int<lower=1, upper=J> sp; // sp index
  vector[N] log_dbh;            // predictor: log(DBH)
  vector[N] log_h;              // response: log(height)
}

parameters {
  real        mu0;         // grand mean of log(mu₀)
  real<lower=0> tau;            // SD of species intercepts
  vector[J]   log_beta0;            // species‐level intercepts = log(β₀ⱼ)
  real        beta1;            // common slope on log(DBH)
  real<lower=0> sigma;          // residual SD
}

model {
  // Likelihood
  log_h ~ normal(log_beta0[sp] + beta1 * log_dbh, sigma);

  // Priors
  mu0 ~ normal(0, 2.5);
  tau      ~ cauchy(0, 1);
  log_beta0  ~ normal(mu0, tau);
  beta1    ~ normal(0, 2.5);
  sigma    ~ cauchy(0, 1);
}

// we need this for model selection
generated quantities {
  vector[N] log_lik;
  for (i in 1:N)
    log_lik[i] = normal_lpdf(log_h[i]
                    | log_beta0[sp[i]] + beta1 * log_dbh[i],
                      sigma);
}
