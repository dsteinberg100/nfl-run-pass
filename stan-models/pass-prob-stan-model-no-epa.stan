data{
  int<lower = 0> N; //number of observations
  int<lower = 1> I; //number of team/seasons
  int<lower = 0, upper = 1> y[N]; //qb_dropback
  int<lower = 0, upper = I> ii[N]; //team/season indicator
  vector[N] phat; //fitted probability from xgboost model
}
parameters{
  vector[I] mu_raw; //team/season random effects
  real beta_phat; //effect of p_hat, should be ~ 1
  real alpha; //intercept
  real<lower = 0> sigma_mu; //standard deviation of random effects
}
transformed parameters{
  vector[I] mu = sigma_mu * mu_raw;
}
model{
  alpha ~ normal(0, .25);
  beta_phat ~ normal(1,.25);
  mu_raw ~ normal(0,1);
  sigma_mu ~ normal(0,1);
  
  y ~ bernoulli_logit(alpha + mu[ii] + beta_phat * phat);
}

