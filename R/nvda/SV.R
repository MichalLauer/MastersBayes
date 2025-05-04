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
symbol <- "NVDA"
from   <- "2022-03-31"
to     <- "2025-03-31"
alpha  <- 0.89

# Prior for phi_raw ------------------------------------------------------------
tf <- function(x) 2 * x - 1
tfi <- function(x) (x + 1) / 2
s1 <- .7
s2 <- .7
lower <- qbeta((1 - alpha)/2, shape1 = s1, shape2 = s2)
lower <- sprintf("%.2f", tf(lower))
upper <- qbeta((1 + alpha)/2, shape1 = s1, shape2 = s2)
upper <- sprintf("%.2f", tf(upper))
tibble(
  x = tf(seq(0, 1, length.out = 1000)),
) |>
  ggplot(aes(x = x)) +
  stat_function(
    fun = \(x, shape1, shape2) dbeta(tfi(x), shape1, shape2),
    args = list(shape1 = s1, shape2 = s2)
  ) +
  stat_function(
    fun = \(x, shape1, shape2) dbeta(tfi(x), shape1, shape2),
    args = list(shape1 = s1, shape2 = s2),
    mapping = aes(fill = "density"),
    xlim = as.numeric(c(lower, upper)),
    geom = "area",
    alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% percentile interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about $\phi$)"),
    subtitle = TeX(paste0(
      "$\\phi \\sim 2 * \\Beta(", s1, ", ", s2, ")$ - 1",
      ", ", alpha*100, "% PI: (", lower, ", ", upper, ")"
    )),
    x = TeX(r"($\phi$)"),
    y = TeX(r"(P($phi$))")
  )

ggsave(filename = "./img/nvda/sv/apriori_phi.png",
       width = 1920, height = 1080, units = "px")

# Prior for phi_raw ------------------------------------------------------------
tf <- function(x) 2 * x - 1
tfi <- function(x) (x + 1) / 2
s1 <- .7
s2 <- .7
lower <- qbeta((1 - alpha)/2, shape1 = s1, shape2 = s2)
lower <- sprintf("%.2f", tf(lower))
upper <- qbeta((1 + alpha)/2, shape1 = s1, shape2 = s2)
upper <- sprintf("%.2f", tf(upper))
tibble(
  x = tf(seq(0, 1, length.out = 1000)),
) |>
  ggplot(aes(x = x)) +
  stat_function(
    fun = \(x, shape1, shape2) dbeta(tfi(x), shape1, shape2),
    args = list(shape1 = s1, shape2 = s2)
  ) +
  stat_function(
    fun = \(x, shape1, shape2) dbeta(tfi(x), shape1, shape2),
    args = list(shape1 = s1, shape2 = s2),
    mapping = aes(fill = "density"),
    xlim = as.numeric(c(lower, upper)),
    geom = "area",
    alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% percentile interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about $\phi$)"),
    subtitle = TeX(paste0(
      "$\\phi \\sim 2 * \\Beta(", s1, ", ", s2, ")$ - 1",
      ", ", alpha*100, "% PI: (", lower, ", ", upper, ")"
    )),
    x = TeX(r"($\phi$)"),
    y = TeX(r"(P($phi$))")
  )

ggsave(filename = "./img/nvda/sv/apriori_phi.png",
       width = 1920, height = 1080, units = "px")

# Prior for mu -----------------------------------------------------------------
tf <- \(p) (p + 1)/2
lower <- qnorm(tf((1 - alpha)/2))
lower <- sprintf("%.2f", lower)
upper <- qnorm(tf((1 + alpha)/2))
upper <- sprintf("%.2f", tf(upper))
tibble(
  x = seq(0, 4, length.out = 1000),
) |>
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm) +
  stat_function(
    fun =dnorm,
    mapping = aes(fill = "density"),
    xlim = as.numeric(c(lower, upper)),
    geom = "area",
    alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% percentile interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about $\mu$)"),
    subtitle = TeX(paste0(
      "$\\mu \\sim Half-N(0, 1)$",
      ", ", alpha*100, "% PI: (", lower, ", ", upper, ")"
    )),
    x = TeX(r"($\mu$)"),
    y = TeX(r"(P($mu$))")
  )

ggsave(filename = "./img/nvda/sv/apriori_mu.png",
       width = 1920, height = 1080, units = "px")

# Prior for nu -----------------------------------------------------------------
sh <- 4.5
rt <- 0.2
lower <- qgamma((1 - alpha)/2, shape = sh, rate = rt)
lower <- sprintf("%.2f", lower)
upper <- qgamma((1 + alpha)/2, shape = sh, rate = rt)
upper <- sprintf("%.2f", upper)
tibble(x = seq(0, 60, length.out = 5000)) |>
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = sh, rate = rt)) +
  stat_function(fun = dgamma,
                aes(fill = "density"),
                args = list(shape = sh, rate = rt),
                xlim = c(lower, upper),
                geom = "area",
                alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% percentile interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about parameters $\nu$)"),
    subtitle = TeX(paste0(
      "$\\nu \\sim \\Gamma(\\alpha = ", sh, ", \\beta = ", rt, ")$",
      ", ", alpha*100, "% PI: (", lower, ", ", upper, ")"
    )),
    x = TeX(r"($\nu$)"),
    y = TeX(r"(P($\nu$))")
  )

ggsave(filename = "./img/nvda/sv/apriori_nu.png",
       width = 1920, height = 1080, units = "px")

# Get and plot data ------------------------------------------------------------
data <-
  symbol |>
  getSymbols(from = from, to = to, auto.assign = FALSE) |>
  _[, 1]

# Get and plot returns
ret <- log(data$NVDA.Open/lag(data$NVDA.Open))[-1]

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
# saveRDS(model, file = "./R/nvda/data/SV.RDS")
# model <- readRDS("./R/nvda/data/SV.RDS")

# Model statistics -------------------------------------------------------------
as_tibble(
  summary(model, pars = c("phi", "mu", "nu_in", "nu_la"))$summary,
  rownames = "param"
) |>
  select(param, Rhat, n_eff) |>
  mutate(
    Rhat = sprintf("%.2f", Rhat),
    n_eff = format(n_eff, big.mark = " ", digits = 1),
    param = ordered(
      param,
      levels = c("phi", "mu", "nu_in", "nu_la")
    )
  ) |>
  arrange(param) |>
  kable(caption = "NVDA/SV: Shrinkage factors and ESS.")

# Compare phi ------------------------------------------------------------------
extract(model, pars = c("phi")) |>
  as_tibble() |>
  ggplot(aes(x = phi)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior density of $\phi$)"),
    y = TeX(r"(P(\phi|x))"),
    x = TeX(r"(\phi)")
  )

ggsave(filename = "./img/nvda/sv/posterior_phi.png",
       width = 1920, height = 1080, units = "px")

extract(model, pars = c("phi")) |>
  as_tibble() |>
  rename("x" = phi) |>
  summarise(
    Average = mean(x),
    SD = sd(x),
    CI_lower = quantile(x, (1 - alpha)/2),
    CI_upper = quantile(x, (1 + alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.4f", x)
    )
  ) |>
  kable(caption = r"(NVDA/GARCH: Tabular description of posterior for $\phi$)")

# Compare mu -------------------------------------------------------------------
extract(model, pars = c("mu")) |>
  as_tibble() |>
  ggplot(aes(x = mu)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior density of $\mu$)"),
    y = TeX(r"(P(\mu|x))"),
    x = TeX(r"(\mu)")
  )

ggsave(filename = "./img/nvda/sv/posterior_mu.png",
       width = 1920, height = 1080, units = "px")

extract(model, pars = c("mu")) |>
  as_tibble() |>
  rename("x" = mu) |>
  summarise(
    Average = mean(x),
    SD = sd(x),
    CI_lower = quantile(x, (1 - alpha)/2),
    CI_upper = quantile(x, (1 + alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.4f", x)
    )
  ) |>
  kable(caption = r"(NVDA/SV: Tabular description of posterior for $\mu$)")

# Compare degrees of freedom ---------------------------------------------------
extract(model, pars = c("nu_la", "nu_in")) |>
  as_tibble() |>
  pivot_longer(
    cols = everything(),
    names_to = "Parameter"
  ) |>
  ggplot(aes(x = value, color = Parameter)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\nu$)"),
    y = TeX(r"(P(\nu|x))"),
    x = TeX(r"(\nu)")
  )

ggsave(filename = "./img/nvda/sv/posterior_nu.png",
       width = 1920, height = 1080, units = "px")

extract(model, pars = c("nu_la", "nu_in")) |>
  as_tibble() |>
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "x"
  ) |>
  group_by(Parameter) |>
  summarise(
    Average = mean(x),
    SD = sd(x),
    CI_lower = quantile(x, (1 - alpha)/2),
    CI_upper = quantile(x, (1 + alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.4f", x)
    )
  ) |>
  kable(caption = r"(NVDA/SV: Tabular description of posteriors for $\nu$)")

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

ggsave(filename = "./img/nvda/sv/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

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
  geom_ribbon(aes(ymin = l, ymax = u), fill = "gray80") +
  geom_line(aes(y = true), color = "black") +
  theme_bw() +
  labs(
    title = TeX(r"(Comparison of posterior distributions)"),
    subtitle = TeX(r"(with shaded 89% percentile interval)"),
    x = NULL, y = TeX(r"($p$)")
  )

ggsave(filename = "./img/nvda/sv/posterior_prediction.png",
       width = 1920, height = 1080, units = "px")
