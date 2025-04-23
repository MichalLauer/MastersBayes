library(rstan)
library(quantmod)
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
      m = 3,
      y = y
    )
  )


summary(model)$summary |> names()
