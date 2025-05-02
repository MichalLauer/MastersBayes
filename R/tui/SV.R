library(quantmod)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(latex2exp)
library(rstan)
library(loo)

# Setup ------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "en")
options(knitr.kable.NA = ' - ')
symbol <- "TUI1.DE"
from   <- "2022-03-31"
to     <- "2025-03-31"
alpha  <- 0.89

# Get and plot data ------------------------------------------------------------
data <-
  symbol |>
  getSymbols(from = from, to = to, auto.assign = FALSE) |>
  _[, 1]
png("./img/tui/closing.png", width = 1920, height = 1080, res = 200)
plot(data, main = "Closing price for $TUI1.DE")
dev.off()

# Get and plot returns
ret <- log(data$TUI1.DE.Open/lag(data$TUI1.DE.Open))[-1]
png("./img/tui/logret.png", width = 1920, height = 1080, res = 200)
plot(ret, ylim = c(-0.16, 0.16), main = "Log returns for $TUI1.DE")
points(ret[which.min(ret)], col = "red", pch = 4, lwd = 2)
points(ret[which.max(ret)], col = "green", pch = 4, lwd = 2)
dev.off()

# Correlation ------------------------------------------------------------------
png("./img/tui/pacf.png", width = 1920, height = 1080, res = 200)
par(mfrow = c(2,2))
pacf(ret^2, main = TeX(r"(Partial ACF of $p^2$)"))
acf(ret^2, main = TeX(r"(ACF of $p^2$)"))
pacf(abs(ret), main = TeX(r"(Partial ACF of $|p|$)"))
acf(abs(ret), main = TeX(r"(ACF of $|p|$)"))
par(mfrow = c(1, 1))
dev.off()


# Model estimation -------------------------------------------------------------
y <-
  coredata(ret) |>
  as.vector()

get_model <- function(p, q, name, data) {
  stan(
    file = "./stan/SV.stan",
    model_name = "SV",
    data = data,
    seed = 1,
    iter = 8000,
    warmup = 2000,
    cores = parallel::detectCores() - 1
  )
}

data <- list(
  T = length(y),
  y = y
)

a <- Sys.time()
model <- get_model(data = data)
print(Sys.time() - a)
saveRDS(model, file = "./R/tui/data/SV.RDS")
# models <- readRDS("./R/nvda/data/SV.RDS")
# Model statistics -------------------------------------------------------------
as_tibble(
  summary(model, pars = c("phi", "mu", "nu_in", "nu_la"))$summary,
  rownames = "param"
) |>
  select(param, Rhat, n_eff) |>
  mutate(
    param = ordered(
      param,
      levels = c("phi", "mu", "nu_in", "nu_la")
    )
  ) |>
  arrange(param) |>
  kable(caption = "TUI1.DE/SV: Shrinkage factors and ESS.")


# Model posteriors -------------------------------------------------------------
extract(model, pars = c("phi", "mu", "nu_in", "nu_la")) |>
  as_tibble() |>
  pivot_longer(
    cols = everything()
  ) |>
  group_by(name) |>
  summarise(
    `E(X)` = mean(value),
    Median = median(value),
    PI_lower = quantile(value, probs = (1 - alpha) / 2),
    PI_upper = quantile(value, probs = (1 + alpha) / 2)
  ) |>
  mutate(
    name = ordered(
      name,
      levels = c("phi", "mu", "nu_in", "nu_la")
    )
  ) |>
  arrange(name) |>
  kable(caption = "TUI1.DE/SV: Shrinkage factors and ESS.")

# Volatility -------------------------------------------------------------------
tibble(
  index = index(ret),
  x = colMeans(exp(extract(model, pars = "h")$h / 2))
) |>
  ggplot(aes(x = index, y = x)) +
  geom_line(show.legend = FALSE) +
  theme_bw() +
  labs(
    title = "Comparison of estimated volatility",
    x = NULL,
    y = TeX(r"($h$)")
  )

ggsave(filename = "./img/tui/sv/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

# Model posteriors -------------------------------------------------------------
tibble(
  Index = index(ret),
  true = coredata(ret),
  l = apply(
    extract(model, pars = "y_pred")$y_pred,
    2,
    \(x) quantile(x, (1 - alpha)/2)
  ),
  u = apply(
    extract(model, pars = "y_pred")$y_pred,
    2,
    \(x) quantile(x, (1 + alpha)/2)
  )
)

# Prediction -------------------------------------------------------------------
df_pred <-
  tibble(
    Index = index(ret),
    true = coredata(ret),
    l = apply(
      extract(model, pars = "y_pred")$y_pred,
      2,
      \(x) quantile(x, (1 - alpha)/2)
    ),
    u = apply(
      extract(model, pars = "y_pred")$y_pred,
      2,
      \(x) quantile(x, (1 + alpha)/2)
    )
  )

df_pred |>
  ggplot(aes(x = Index)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.7) +
  geom_line(aes(y = true)) +
  theme_bw() +
  labs(
    title = TeX(r"(Comparison of posterior distributions for $\exp(h/2)$)"),
    subtitle = TeX(r"(with shaded 89% percentile interval)"),
    x = NULL, y = TeX(r"($\exp(h/2)$)")
  )
