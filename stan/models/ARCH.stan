data {
  int<lower=1> T; // Length of the TS
  int<lower=1> m; // Order of ARCH(m)
  vector[T] y;    // Log of returns
}

parameters {
  real<lower=0> alpha0;          // Intercept
  vector<lower=0>[m] real_alpha; // Coefficients
  real<lower=1> nu;              // Degrees of freedom
}

transformed parameters {
  vector[T - m] sigma2;      // conditional variances
  vector[m] alpha = real_alpha / (1 + sum(real_alpha));

  for (t in (m + 1):T) {
    real tmp = alpha0;
    for (j in 1:m) {
      tmp += alpha[j] * square(y[t - j]);
    }
    sigma2[t - m] = tmp;
  }
}

model {
  alpha0      ~ gamma(2, 2.5);
  real_alpha  ~ gamma(2, 2.5);
  nu          ~ gamma(4, 0.3);

  for (i in 1:(T-m)) {
    y[i + m] ~ student_t(nu, 0, sqrt(sigma2[i]));
  }
}

generated quantities {
  vector[T - m] y_pred;

  for (i in (m + 1):T) {
    real temp = alpha0;
    for (j in 1:m) {
      temp += alpha[j] * square(y[i - j]);
    }

    y_pred[i - m] = student_t_rng(nu, 0, sqrt(temp));
  }
}
