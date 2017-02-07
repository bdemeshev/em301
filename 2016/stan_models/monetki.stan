data {
  int N; // количество наблюдений
  int y[N]; // 0 или 1, данные о вытащенных монетках
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  // априорное мнение о параметрах:
  p ~ uniform(0, 1); 
  // устройство данных:
  for (n in 1:N) {
    y[n] ~ bernoulli(p);
  }
}
