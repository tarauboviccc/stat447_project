# Bayesian Logistic Regression Model
data {
  int<lower = 0> N; # number of transactions
  int<lower = 0> P; # number of categories
  int<lower = 0,upper = 1> y[N]; # purchase indicator
  matrix[N,P] X; // product category presence matrix
}
parameters {
  vector[P] beta; // cross-effect coefficients
  real<lower=0> sigma;
}
model {
  beta ~ normal(0, sigma);
  sigma ~ exponential(1);
  
  y ~ bernoulli_logit(X * beta);
}
