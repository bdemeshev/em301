// probabilistic programming

data {
  int N; // число наблюдений
  int<lower=0, upper=1> y[N]; // вектор наблюдений
}
parameters {
  real<lower=0, upper=1> prob; // доля золотых монеток в шапке
}
model {
  // априорное мнение о параметрах
  prob ~ uniform(0, 1);
  // мнение об устройстве мира
  for (n in 1:N) {
    y[n] ~ bernoulli(prob);
  }
}
generated quantities {
  int y_new; // прогноз, какая монетка вылезет из шляпы в след раз
  y_new = bernoulli_rng(prob);
}


