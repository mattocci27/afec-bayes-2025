// observed data
data {
  int<lower=0> N;
  vector[N] y;
}

// parameters to be estimated
parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  // likelihood
  y ~ normal(mu, sigma);
  // priors
  mu ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
}
