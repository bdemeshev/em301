data {
  int N; // количество наблюдений
  real y[N]; // где Медведи чуют Хариса
}

parameters {
  real h; // точка на прямой, где спрятался Харис
}

model {
  // априорное мнение о параметрах:
  h ~ normal(7, 100);
  // устройство данных:
  for (n in 1:N) {
    y[n] ~ normal(h, 10);
  }
}

generated quantities {
  real y_new; // прогноз места, где учует Хариса новый Медведь
  y_new = normal_rng(h, 10);
}

