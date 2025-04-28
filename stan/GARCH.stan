data {
  int<lower=1> T; // Length of the TS
  int<lower=1> m; // Order of GARCH(m, q)
  int<lower=1> q; // Order of GARCH(m, q)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0;     // Intercept
  simplex[m + q + 1] coefs;         // All coefs
  real<lower=1> nu;         // Degrees of freedom
}

transformed parameters {
  vector[T - m] sigma2 = rep_vector(alpha0, T - m);
  vector[m] alpha = coefs[1:m];
  vector[q] beta = coefs[(m+1):(m + q)];

  for (t in 1:(T - m)) {
    for (j in 1:m) {
      sigma2[t] += alpha[j] * square(y[t + m - j]);
    }
  }

  for (t in (q + 1):(T-m)) {
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

  for (i in (q + 1):(T-m)) {
    y[i + m] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  vector[T - m] total = sum(alpha) + sum(beta);
  vector[T - m] y_pred;

  for (i in (m + 1):T) {
    real temp = alpha0;

    for (t in 1:(T - m)) {
      for (j in 1:m) {
        temp += alpha[j] * square(y[t + m - j]);
      }
    }

    for (t in (q + 1):(T-m)) {
      for (i in 1:q) {
        sigma2[t] += beta[i] * sigma2[t - i];
      }
    }

    y_pred[i - m] = student_t_rng(nu, 0, sqrt(temp));
  }
}
