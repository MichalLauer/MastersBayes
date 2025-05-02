data {
  int<lower=1> T; // Length of the TS
  int<lower=1> p; // Order of GARCH(p, q)
  int<lower=1> q; // Order of GARCH(p, q)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0;     // Intercept
  simplex[p + q + 1] coefs; // All coefs
  real<lower=1> nu;         // Degrees of freedom
}

transformed parameters {
  vector[T - p] sigma2 = rep_vector(alpha0, T - p);  // Conditional variance
  vector[p] alpha = coefs[1:p];                      // Coefficients
  vector[q] beta coefs[(p+1):(p + q)];               // Coefficients

  // Add past residuals
  for (t in 1:(T - p)) {
    for (j in 1:p) {
      sigma2[t] += alpha[j] * square(y[t + p - j]);
    }
  }

  // Add past conditional variance
  for (t in (q + 1):(T - p)) {
    for (i in 1:q) {
      sigma2[t] += beta[i] * sigma2[t - i];
    }
  }
}

model {
  nu     ~ gamma(4, 0.4);
  alpha0 ~ gamma(2, 4);
  alpha  ~ beta(2, 2);
  beta   ~ beta(2, 2);

  for (i in (q + 1):(T-p)) {
    y[i + p] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  real total = sum(alpha) + sum(beta);
  vector[T - p] log_lik;
  vector[T - p] y_pred;

  for (i in 1:(T - p)) {
    y_pred[i] = student_t_rng(nu, 0, sqrt(sigma2[i]));
    log_lik[i] = student_t_lpdf(y[i + p] | nu, 0, sqrt(sigma2[i]));
  }

}
