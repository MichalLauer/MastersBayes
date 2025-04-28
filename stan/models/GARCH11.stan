data {
  int<lower=1> T; // Length of the TS
  vector[T] y;    // Log of returns
  real s2;        // Initial variance
}

parameters {
  real<lower=0> alpha0;     // Intercept
  real<lower=0, upper=1> alpha1;
  real<lower=0, upper=(1-alpha1)> beta1;
  real<lower=1> nu;         // Degrees of freedom
}

transformed parameters {
  array[T] real<lower=0> sigma2;
  sigma2[1] = s2;
  for (t in 2:T) {
    sigma2[t] = alpha0
                     + alpha1 * pow(y[t - 1], 2)
                     + beta1 * sigma2[t - 1];
  }
}

model {
  alpha0 ~ gamma(2, 2.5);
  alpha1  ~ gamma(2, 2.5);
  beta1   ~ gamma(2, 2.5);
  nu     ~ gamma(4, 0.3);

  y ~ student_t(nu, 0, sqrt(sigma2));
}

generated quantities {
  array[T] real y_pred;

  y_pred[1] = student_t_rng(nu, 0, sqrt(s2));

  for (t in 2:T) {
    real sigma_pred_t = sqrt(alpha0
                            + alpha1 * pow(y[t-1], 2)
                            + beta1 * sigma2[t-1]);

    // Sample from the predictive distribution
    y_pred[t] = student_t_rng(nu, 0, sigma_pred_t);
  }
}
