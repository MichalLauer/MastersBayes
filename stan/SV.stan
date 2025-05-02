data {
  int<lower=1> T;       // Number of time periods
  vector[T] y;          // Observed returns
}

parameters {
  real<lower=0, upper=1> phi_raw; // Persistence of volatility
  real<lower=0> mu;               // Mean of the log volatility
  real<lower=2> nu_la;            // Degrees of freedom for latent volatility
  real<lower=2> nu_in;            // Degrees of freedom for observations
  vector[T] h;                    // Latent log volatilities
}

transformed parameters {
  real<lower=-1,upper=1> phi = 2 * phi_raw - 1;
}

model {
  phi_raw ~ beta(.7, .7);    // Prior for phi, transformed to [-1, 1]
  mu      ~ normal(0, 1);    // Prior for mu
  nu_la   ~ gamma(4.5, 0.2); // Prior for latent df
  nu_in   ~ gamma(4.5, 0.2);  // Prior for observation df

  // Initial state
  h[1] ~ student_t(nu_la, mu, 1 / sqrt(1 - phi * phi));

  // Latent volatility
  for (i in 2:T) {
    h[i] ~ student_t(nu_la, mu + phi * (h[i - 1] - mu), 1);
  }

  // Observation equation
  for (i in 1:T) {
    y[i] ~ student_t(nu_in, 0, exp(h[i] / 2));
  }
}

generated quantities {
  vector[T] y_pred;
  vector[T] log_lik;

  for (i in 1:T) {
    y_pred[i] = student_t_rng(nu_in, 0, exp(h[i] / 2));
    log_lik[i] = student_t_lpdf(y[i] | nu_in, 0, exp(h[i] / 2));
  }
}
