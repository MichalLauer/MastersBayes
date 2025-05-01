data {
  int<lower=1> T; // Length of the TS
  int<lower=1> p; // Order of ARCH(p)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0; // Intercept
  simplex[p + 1] alpha; // Coefficients
  real<lower=1> nu;     // Degrees of freedom
}

transformed parameters {
  vector[T - p] sigma2 = rep_vector(alpha0, T - p);

  for (i in 1:(T - p)) {
    for (j in 1:p) {
      sigma2[i] += alpha[j] * square(y[i + p - j]); // Conditional variance
    }
  }
}

model {
  nu     ~ gamma(4, 0.4);
  alpha0 ~ gamma(2, 4);
  alpha  ~ beta(2, 6);

  for (i in 1:(T - p)) {
    y[i + p] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  real total = sum(alpha[1:p]);
  vector[T - p] log_lik;
  vector[T - p] y_pred;

  for (i in 1:(T - p)) {
    y_pred[i] = student_t_rng(nu, 0, sqrt(sigma2[i]));
    log_lik[i] = student_t_lpdf(y[i + p] | nu, 0, sqrt(sigma2[i]));
  }
}
