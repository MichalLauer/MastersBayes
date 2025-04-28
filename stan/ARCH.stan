data {
  int<lower=1> T; // Length of the TS
  int<lower=1> m; // Order of ARCH(m)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0; // Intercept
  simplex[m + 1] alpha; // Coefficients
  real<lower=1> nu;     // Degrees of freedom
}

transformed parameters {
  vector[T - m] sigma2 = rep_vector(alpha0, T - m);

  for (t in 1:(T - m)) {
    for (j in 1:m) {
      sigma2[t] += alpha[j] * square(y[t + m - j]); // Conditional variance
    }
  }
}

model {
  nu     ~ gamma(4, 0.4);
  alpha0 ~ gamma(2, 4);
  alpha  ~ beta(2, 6);

  for (i in 1:(T-m)) {
    y[i + m] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  real total = sum(alpha[1:m]);
  vector[T - m] y_pred;

  for (i in 1:(T - m)) {
    real temp = alpha0;
    for (j in 1:m) {
      temp += alpha[j] * square(y[i + m - j]);
    }
    y_pred[i] = student_t_rng(nu, 0, sqrt(temp));
  }
}
