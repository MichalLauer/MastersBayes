data {
  int<lower=1> T; // Length of the time series
  int<lower=1> m; // GARCH(m, q) - ARCH order
  int<lower=1> q; // GARCH(m, q) - GARCH order
  vector[T] y;    // Returns (log returns)
}

transformed data {
  int max_lag = max(m, q); // Should be int, not real
}

parameters {
  real<lower=0> alpha0;      // Intercept
  vector<lower=0>[m] alpha;  // ARCH coefficients
  vector<lower=0>[q] beta;   // GARCH coefficients
  real<lower=1> nu;          // Degrees of freedom for Student-t
}

transformed parameters {
  vector[T - max_lag] sigma2;      // conditional variances

  for (t in (max_lag + 1):T) {
    real tmp = alpha0;
    for (j in 1:m) {
      tmp += alpha[j] * square(y[t - j]);
    }
    for (j in 1:q) {
      tmp += beta[j] * sigma2[t - j];
    }
    sigma2[t - max_lag] = tmp;
  }
}

model {
  alpha0 ~ gamma(2, 2.5);
  alpha  ~ gamma(2, 2.5);
  beta   ~ gamma(2, 2.5);
  nu     ~ gamma(4, 0.3);

  for (t in (max_lag + 1):T) {
    y[t] ~ student_t(nu, 0, sqrt(sigma2[t]));
  }
}
