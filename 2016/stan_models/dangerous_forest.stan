data {
  int y[3]; // количество встреч
  // c Волком/БЯ/Харисом
  
  int N; // количество дней
}
parameters {
  real<lower=0, upper=0.5> alpha;
}
transformed parameters {
  simplex[3] prob; //  элемент симплекса, вектор из вероятностей
  prob[1] = alpha;
  prob[2] = alpha;
  prob[3] = 1 - 2 * alpha;
}
model {
  y ~ multinomial(prob);
}



