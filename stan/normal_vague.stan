data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
  mu ~ normal(0, 1e+4); // Vague prior for mu
  sigma ~ normal(0, 1e+4); // Vague prior for sigma
}
