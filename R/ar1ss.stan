data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0> rho_sd;
  int<lower=1> start_observer_effect;
}
parameters {
  real alpha;
  real<lower=-1, upper=1> rho1;
  real<lower=-1, upper=1> rho2;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o1;
  real<lower=0> sigma_o2;
  vector[N] pro_raw;
  real y_init;
  real observer_effect;
}
transformed parameters {
  vector[N] y_true;
  y_true[1] = y_init;
  for (n in 2:N) {
    // process error model (non-centered)
    if (n < start_observer_effect) {
      y_true[n] = alpha + rho1 * y_true[n-1] + pro_raw[n] * sigma_p;
    } else {
      y_true[n] = alpha + observer_effect + rho2 * y_true[n-1] + pro_raw[n] * sigma_p;
    }
  }
}
model {
  pro_raw ~ std_normal();
  for (n in 1:N) {
    if (n < start_observer_effect) {
      y[n] ~ normal(y_true[n], sigma_o1);
    } else {
      y[n] ~ normal(y_true[n], sigma_o2);
    }
  }
  rho1 ~ normal(0, rho_sd);
  rho2 ~ normal(0, rho_sd);
  sigma_o1 ~ normal(0, 1);
  sigma_o2 ~ normal(0, 1);
  sigma_p ~ normal(0, 1);
  alpha ~ normal(0, 1);
  y_init ~ normal(0, 5);
  observer_effect ~ normal(0, 1);
}
