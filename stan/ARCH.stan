data {
  int<lower=1> T; // Length of the TS
  vector[T] y;    //Log of returns
}

transformed data {
  vector[T] resid;
  resid = square(y);
}

parameters {
  real<lower=0> alpha0;         // Intercept
  real<lower=0, upper=1> alpha; // Coefficients
  real<lower=1> epsilon_nu;
}

model {
  alpha0     ~ gamma(1, 1);
  alpha      ~ beta(1, 1);
  epsilon_nu ~ gamma(2, 0.2);

  for (t in 2:T) {
    real sigma_t = sqrt(alpha0 + alpha * pow(resid[t-1], 2));
    y[t] ~ student_t(epsilon_nu, 0, sigma_t);
  }

}

generated quantities {
  vector[T] sigma;
  sigma[1] = sqrt(alpha0);  // Initial volatility (assuming no lag for t=1)
  for (t in 2:T) {
    sigma[t] = sqrt(alpha0 + alpha * square(y[t-1]));
  }
}
