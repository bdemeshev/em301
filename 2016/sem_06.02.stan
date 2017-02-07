#Общая схема:
  #1) данные
  #2) параметры модели
  #3) модель зависимости
data{
  int y[3]; // количество встреч с волком, бобой ягой и Харисом
  int N; // количество дней
}

parameters{
  real<lower = 0, upper = 0.5> alpha;
}

transformed parameters {
  simplex[3] prob; // элемент симлекса, вектор из вероятностей
  prob[1] = alpha;
  prob[2] = alpha;
  prob[3] = 1 - 2 * alpha;
}
model{
   y ~ multinomial(prob);
}
