data {
  int y[3]; // количество Медовиков/Штруделей/БанТворогов
}
parameters {
  real<lower=0, upper=1.0/3> a; // вероятность Штруделя
}
transformed parameters {
  simplex[3] prob; // вектор из трёх вероятностей
  prob[1] = 2 * a;
  prob[2] = a;
  prob[3] = 1 - 3 * a;
}
model {
  //  априорное мнение об a
  a ~ uniform(0, 1.0/3); 
  // модель для наблюдаемых величин
  y ~ multinomial(prob);
}
generated quantities {
  // прогноз следующего наблюдения
  int y_new[3];
  y_new = multinomial_rng(prob, 1);
}
