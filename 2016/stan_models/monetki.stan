data {
  int N; // number of observations
  int y[N]; // 0 or 1, observed color of a coin
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  // prior info about parameters:
  p ~ uniform(0, 1);
  // model for observed data:
  for (n in 1:N) {
    y[n] ~ bernoulli(p);
  }
}
