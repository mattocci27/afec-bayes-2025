data {
  int<lower=0> N;
  vector[N] y;
  matrix[3, N] x;
}

parameters {
  row_vector[3] beta;
  real<lower=0> sigma;
}

model {
  y ~ normal(beta * x, sigma);
  beta ~ normal(0, 2.5);
  sigma ~ cauchy(0, 2.5);
}
