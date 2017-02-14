data {
  int<lower=1> N; // количество наблюдений
  real y[N]; // численность населений
  int<lower=1> H; // число шагов прогноза вперёд
}
parameters {
  real beta_1;
  real beta_2;
  real<lower=0> sigma;
}
model {
  // prior distribution
  beta_1 ~ normal(0, 10^3);
  beta_2 ~ normal(1, 0.5);
  sigma ~ exponential(0.001);
  // model
  for (n in 2:N) {
    y[n] ~ normal(
      beta_1 + beta_2 * y[n - 1], sigma);
  }
}
generated quantities {
  real y_new[H]; // forecasts
  y_new[1] = normal_rng(
    beta_1 + beta_2 * y[N], sigma);
  for (h in 2:H) {
    y_new[h] = normal_rng(
      beta_1 + beta_2 * y_new[h - 1], sigma);
  }
}

