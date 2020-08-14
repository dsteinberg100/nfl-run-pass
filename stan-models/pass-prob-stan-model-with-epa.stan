data{
  int<lower = 0> N; //number of observations
  int<lower = 0> K; //number of predictors
  int<lower = 1> I; //number of team/seasons
  int<lower = 0, upper = 1> y[N]; //qb_dropback
  int<lower = 0, upper = I> ii[N]; //team/season indicator
  matrix[N,K] x; //predictors
}
parameters{
  vector[I] mu_raw; //team/season random effects
  vector[K] beta; //vector of coefficients
  real alpha; //intercept
  real<lower = 0> sigma_mu; //standard deviation of random effects
}
transformed parameters{
  vector[I] mu = sigma_mu * mu_raw;
}
model{
  alpha ~ normal(0, .25);
  beta[1] ~ normal(1,.25); // prior on effect of p_hat
  beta[2] ~ normal(0, 1); //prior on effect of epa difference
  mu_raw ~ normal(0,1);
  sigma_mu ~ normal(0,1);
  
  y ~ bernoulli_logit(alpha + mu[ii] + x * beta);
}

