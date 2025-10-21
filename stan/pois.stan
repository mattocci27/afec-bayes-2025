data {
  int<lower=0> N;
  array[N] int<lower=0> y;
}

parameters {
  real<lower=0> lambda;
}

model {
  y ~ poisson(lambda);
  lambda ~ normal(0, 10);
}
