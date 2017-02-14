data {
  int N; // число наблюдений
  int y[N]; // отдельные наблюдения
}
parameters {
  real<lower=0, upper=1> p; 
  // доля золотых монет в шляпе
}
model {
  // априорно:
  p ~ uniform(0, 1);
  // модель: как наблюдения связаны с параметром
  for (n in 1:N) {
    y[n] ~ bernoulli(p);
  }
}
generated quantities {
  int y_new; // какая монетка следующая?
  y_new = bernoulli_rng(p); // генерация будущего игрека
}
