data {
  int<lower=0> N;
  array[N] int<lower=0> y;
}

parameters {
  real log_lambda;
}

model {
  y ~ poisson_log(log_lambda);
  log_lambda ~ normal(0, 2.5);
}
