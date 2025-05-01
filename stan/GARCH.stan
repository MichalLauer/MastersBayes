data {
  int<lower=1> T; // Length of the TS
  int<lower=1> p; // Order of GARCH(p, q)
  int<lower=1> q; // Order of GARCH(p, q)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0;     // Intercept
  simplex[p + q + 1] coefs; // All coefs
  vector[p] alpha;
  vector[q] beta;
  real<lower=1> nu;         // Degrees of freedom
}

transformed parameters {
  vector[T - p] sigma2 = rep_vector(alpha0, T - p);
  alpha = coefs[1:p];
  beta coefs[(p+1):(p + q)];

  for (t in 1:(T - p)) {
    for (j in 1:p) {
      sigma2[t] += alpha[j] * square(y[t + p - j]);
    }
  }

  for (t in (q + 1):(T - p)) {
    for (i in 1:q) {
      sigma2[t] += beta[i] * sigma2[t - i];
    }
  }
}

model {
  alpha0 ~ gamma(2, 2.5);
  alpha  ~ gamma(2, 2.5);
  beta   ~ gamma(2, 2.5);
  nu     ~ gamma(4, 0.3);

  for (i in (q + 1):(T-p)) {
    y[i + p] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  vector[T - p] total = sum(alpha) + sum(beta);
  vector[T - p] y_pred;

  for (t in 1:(T - p)) {
    y_pred[t] = student_t_rng(nu, 0, sqrt(sigma2[t]))
  }

}
