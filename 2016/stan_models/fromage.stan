data {
  int N; // количество мышей
  real y[N]; // где мышки чуют сыр сильнее всего
}
parameters {
  real m; // место, где спрятан сыр
  real<lower=0> nose; // заложенность носа мышки
}
model {
  // априорное мнение
  m ~ normal(13, 10); 
  nose ~ exponential(0.05);
  // как мыши чуют сыр:
  for (n in 1:N) {
    y[n] ~ normal(m, nose);
  }
}
generated quantities {
  real y_new;
  y_new = normal_rng(m, nose);
}


