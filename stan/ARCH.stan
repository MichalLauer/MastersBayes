data {
  int<lower=1> T; // Length of the TS
  int<lower=1> m; // Order of ARCH(m)
  vector[T] y;    // Log of returns
}

transformed data {
  matrix[T-m, m] residual;

  for (i in 1:(T-m)) {
    for (j in 1:m) {
      residual[i, j] = y[m + i - j]; // Since E(y) = 0, historial values
                                     // are just residuals.
    }
  }
}

parameters {
  real alpha0;
  simplex[m] alpha;
}

transformed parameters {
  vector[T-m] sigma;
  for (i in 1:(T-m)) {
    sigma[i] = alpha0 + dot_product(alpha, square(residual[i,]));
  }
}

model {
  alpha0 ~ gamma(2, 5);
  alpha ~ dirichlet([0.3, 0.3, 0.3]);

  for (i in 1:(T-m)) {
    y[i + m] ~ student_t(5, 0, sigma[i]);
  }
}
