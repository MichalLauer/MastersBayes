library(rstan)
library(tidyquant)
symbol <- "GOOGL"
from   <- "2024-01-01"
to     <- "2025-01-01"

data <-
  symbol |>
  getSymbols(from = from, to = to, auto.assign = FALSE) |>
  _[, 1]
plot(data)

ret <- log(data$GOOGL.Open/lag(data$GOOGL.Open))[-1]
plot(ret)

y <-
  coredata(ret) |>
  as.vector()

model <-
  stan(
    file = "./stan/ARCH.stan",
    data = list(
      T = length(y),
      y = y
    )
  )

sigma <- extract(model, pars = "sigma")$sigma
sigma_xts <- xts(colMeans(sigma), order.by = index(ret))

lines(sigma_xts, col = "red")
lines(-sigma_xts, col = "red")
