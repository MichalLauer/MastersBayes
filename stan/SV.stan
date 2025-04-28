data {
  int<lower=0> T;   // # time points (equally spaced)
  vector[T] y;      // mean corrected return at time t
}

parameters {
  real<lower=-1,upper=1> phi;  // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h;                 // log volatility at time t
  real<lower=1> nu;            // Degrees of freedom
}

model {
  phi ~ beta(20, 1.5);
  sigma ~ normal(0, 0.2);
  nu ~ gamma(2, 0.1);


  h[1] ~ student_t(nu, 0, sigma / sqrt(1 - phi * phi));
  for (t in 2:T)
    h[t] ~ student_t(nu, phi * h[t - 1], sigma);
  for (t in 1:T)
    y[t] ~ student_t(nu, 0, exp(h[t] / 2));
}
