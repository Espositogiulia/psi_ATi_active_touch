data {
  int<lower=1> C; //n conditions
  int<lower=0> N; //n data point
  int<lower=1> I; //n participants
  int<lower=0> y[N]; //participant response
  int<lower=1, upper=I> ii[N]; //participant id
  int<lower=1,upper=3> condition[N]; //conditon
  vector[N] dist; //distance from center
  int<lower=0> nT[N]; //number of presentations
  matrix<lower=0, upper=1>[C,C] x; //predictor matrix
}
parameters {
  vector[C] mu_a;
  vector<lower=0>[C] sigma_a;
  vector[C] mu_log10_b;
  vector<lower=0>[C] sigma_log10_b;
  real mu_logit_l;
  real<lower=0> sigma_logit_l;
  vector[C] delta_coef_a[I];
  vector[C] delta_coef_log10_b[I];
  vector[I] delta_logit_l;
}
transformed parameters{
  vector[I] logit_l;
  vector[C] coef_log10_b[I];
  vector[C] coef_a[I];
  vector[N] theta;
  vector[C] a[I];
  vector<lower=0>[C] b[I];  
  vector<lower=0,upper=.5>[I] l;
  for(i in 1:I){
    coef_a[i,1] = delta_coef_a[i,1];
    coef_a[i,2] = delta_coef_a[i,2];
    coef_a[i,3] = mu_a[3] + sigma_a[3] * delta_coef_a[i,3];
    coef_log10_b[i,1] = delta_coef_log10_b[i,1];
    coef_log10_b[i,2] = mu_log10_b[2] + sigma_log10_b[2]* delta_coef_log10_b[i,2];#
    coef_log10_b[i,3] = mu_log10_b[3] + sigma_log10_b[3]* delta_coef_log10_b[i,3];#
    logit_l[i] = mu_logit_l + sigma_logit_l * delta_logit_l[i];
  }
  for(i in 1:I){
    for(c in 1:C){      
      a[i][c] = x[c] * coef_a[i];# this is for individual fit
      b[i][c] = pow(10,x[c] *coef_log10_b[i]);#
      l[i] = inv_logit(logit_l[i]) / 2;#
    }
  }     
  for(n in 1:N){
      theta[n]= l[ii[n]] + (1 - (2 * l[ii[n]])) * erfc(-b[ii[n]][condition[n]] * (dist[n] - a[ii[n]][condition[n]]) / sqrt(2)) / 2;
  }
}
model {
  mu_a[1] ~ normal(0,1);
  mu_a[2:3] ~ normal(0,.5);
  sigma_a[1] ~ normal(0,1);
  sigma_a[2:3] ~ normal(0,.5);
  mu_log10_b[1] ~ normal(1,.5);
  mu_log10_b[2:3] ~ normal(0,.25);
  sigma_log10_b[1] ~ normal(0,.5);
  sigma_log10_b[2:3] ~ normal(0,.25);
  mu_logit_l ~ normal(-4,1);
  sigma_logit_l ~ std_normal();
  for (i in 1:I){
    delta_coef_a[i,1] ~ normal(mu_a[1],sigma_a[1]);
    delta_coef_a[i,2] ~ normal(mu_a[2],sigma_a[2]);
    delta_coef_a[i,3] ~ std_normal();
    delta_coef_log10_b[i,1] ~ normal(mu_log10_b[1],sigma_log10_b[1]);
    delta_coef_log10_b[i,2] ~ std_normal();
    delta_coef_log10_b[i,3] ~ std_normal();
    delta_logit_l[i] ~ std_normal();
  }
  y ~ binomial (nT, theta);
}
generated quantities{
  vector[N] log_lik;
  for (idx in 1:N) {
    log_lik[idx] = binomial_logit_lpmf(y[idx] | nT[idx],theta[idx]);
  }
}
