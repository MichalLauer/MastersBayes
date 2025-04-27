library(quantmod)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(latex2exp)
library(rstan)

# Setup
Sys.setlocale("LC_ALL", "en")
options(knitr.kable.NA = ' - ')
symbol <- "GOOGL"
from   <- "2022-03-31"
to     <- "2025-03-31"

# Get and plot data
data <-
  symbol |>
  getSymbols(from = from, to = to, auto.assign = FALSE) |>
  _[, 1]

# Get and plot returns
ret <- log(data$GOOGL.Open/lag(data$GOOGL.Open))[-1]

# Apriori for Degrees of Freedom
alpha <- 0.89
sh <- 4
sc <- 3
lower <- qgamma((1-alpha)/2, shape = sh, scale = sc)
upper <- qgamma((1+alpha)/2, shape = sh, scale = sc)
print(paste(lower, upper))
tibble(x = seq(0, 40, length.out = 500)) |>
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = sh, scale = sc)) +
  stat_function(fun = dgamma,
                aes(fill = "density"),
                args = list(shape = sh, scale = sc),
                xlim = c(lower, upper),
                geom = "area",
                alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% credibility interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Apriori knowledge about $\nu$)"),
    subtitle = TeX(paste0(
      "$\\nu \\sim \\Gamma(\\alpha = ", sh, ", \\beta = ", sc, ")$")),
    x = TeX(r"($\nu$)"),
    y = TeX(r"(P($\nu$))")
  )
ggsave(filename = "./img/googl/arch/apriori_nu.png",
       width = 1920, height = 1080, units = "px")


# Apriori for alpha
alpha <- 0.89
sh <- 2
sc <- 0.4
lower <- qgamma((1-alpha)/2, shape = sh, scale = sc)
upper <- qgamma((1+alpha)/2, shape = sh, scale = sc)
print(paste(lower, upper))
tibble(x = seq(0, 10, length.out = 1000)) |>
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = sh, scale = sc)) +
  stat_function(fun = dgamma,
                aes(fill = "density"),
                args = list(shape = sh, scale = sc),
                xlim = c(lower, upper),
                geom = "area",
                alpha = 0.5) +
  scale_fill_manual(name = NULL, values = c("density" = "skyblue"),
                    labels = c("density" = "89% credibility interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Apriori knowledge about $\alpha_i$)"),
    subtitle = TeX(paste0(
      "$\\alpha_i \\sim \\Gamma(\\alpha = ", sh, ", \\beta = ", sc, ")$")),
    x = TeX(r"($\alpha_i $)"),
    y = TeX(r"(P($\alpha_i $))")
  )
ggsave(filename = "./img/googl/arch/apriori_alpha.png",
       width = 1920, height = 1080, units = "px")


# Generate models
y <-
  coredata(ret) |>
  as.vector()

get_model <- function(m, q, data) {
  data$m <- m
  data$q = q

  print(paste("Modeling", m, ":", q))
  stan(
    file = "./stan/GARCH.stan",
    model_name = "GARCH",
    data = data,
    seed = m,
    iter = 8000,
    warmup = 2000
  )
}

data <- list(
  T = length(y),
  y = y
)

a <- Sys.time()
models <-
  expand.grid(
    m = 1:2,
    q = 1:2
  ) |>
  pmap(\(m, q) get_model(m = m, q = q, data = data))
print(Sys.time() - a)

# ------------------------------------------------------------------------------
# Model statistics
# ------------------------------------------------------------------------------
map(1:8, \(x) {
  as_tibble(
    summary(models[[x]], pars = c("nu", "alpha", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(m = x)
})
x |>
  bind_rows() |>
  select(m, param, Rhat) |>
  mutate(
    m = paste("m =", m),
    param = ordered(param,
                    levels = c("nu", "alpha0",
                               paste0("alpha[", 1:8, "]")))

  ) |>
  pivot_wider(
    names_from = m,
    values_from = Rhat
  ) |>
  arrange(param) |>
  kable(caption = "GOOGL/ARCH: Shrinkage factors")

map(1:8, \(x) {
  as_tibble(
    summary(models[[x]], pars = c("nu", "alpha", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(m = x)
}) |>
  bind_rows() |>
  select(m, param, n_eff) |>
  mutate(
    m = paste("m =", m),
    param = ordered(param,
                    levels = c("nu", "alpha0",
                               paste0("alpha[", 1:8, "]")))

  ) |>
  pivot_wider(
    names_from = m,
    values_from = n_eff
  ) |>
  arrange(param) |>
  kable(caption = "GOOGL/ARCH: ESS")

# ------------------------------------------------------------------------------
# Compare degrees of freedom
# ------------------------------------------------------------------------------
map(1:8, \(x) {
  tibble(
    arch = x,
    nu = as.numeric(rstan::extract(models[[x]], pars = "nu")$nu)
  )
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch))
  ) |>
  ggplot(aes(x = nu, color = `Arch(m)`)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\nu$)"),
    y = TeX(r"(P(\nu|x))"),
    x = TeX(r"(\nu)")
  )

ggsave(filename = "./img/googl/arch/posterior_nu.png",
       width = 1920, height = 1080, units = "px")

alpha <- 0.89
map(1:8, \(x) {
  tibble(
    arch = x,
    nu = as.numeric(rstan::extract(models[[x]], pars = "nu")$nu)
  )
}) |>
  bind_rows() |>
  group_by(arch) |>
  summarise(
    Average = round(mean(nu), 2),
    SD = round(sd(nu), 2),
    CI_lower = quantile(nu, (1-alpha)/2),
    CI_upper = quantile(nu, (1+alpha)/2)
  ) |>
  mutate(
    arch = paste("m =", arch),
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -arch,
    names_to = "Arch(m)"
  ) |>
  pivot_wider(
    names_from = "arch"
  ) |>
  kable(caption = "GOOGL/ARCH: Tabular description of posterior for $\nu$")

# ------------------------------------------------------------------------------
# Compare intercept
# ------------------------------------------------------------------------------
map(1:8, \(x) {
  tibble(
    arch = x,
    a0 = as.numeric(rstan::extract(models[[x]], pars = "alpha0")$alpha0)
  )
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch))
  ) |>
  ggplot(aes(x = a0, color = `Arch(m)`)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\alpha_0$)"),
    y = TeX(r"($P(\alpha_0|x)$)"),
    x = TeX(r"($\alpha_0$)")
  )

ggsave(filename = "./img/googl/arch/posterior_alpha0.png",
       width = 1920, height = 1080, units = "px")

alpha <- 0.89
map(1:8, \(x) {
  tibble(
    arch = x,
    a0 = as.numeric(rstan::extract(models[[x]], pars = "alpha0")$alpha0)
  )
}) |>
  bind_rows() |>
  group_by(arch) |>
  summarise(
    Average = round(mean(a0), 2),
    SD = round(sd(a0), 2),
    CI_lower = quantile(a0, (1-alpha)/2),
    CI_upper = quantile(a0, (1+alpha)/2)
  ) |>
  mutate(
    arch = paste("m =", arch),
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -arch,
    names_to = "Arch(m)"
  ) |>
  pivot_wider(
    names_from = "arch"
  ) |>
  kable(caption = "GOOGL/ARCH: Tabular description of posterior for $\alpha_0$")

# ------------------------------------------------------------------------------
# Compare alphas
# ------------------------------------------------------------------------------
i <- 1
map(i:8, \(x) {
  tibble(
    arch = x,
    a = as.numeric(rstan::extract(models[[x]], pars = "alpha_tr")$alpha_tr[, i])
  )
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch))
  ) |>
  ggplot(aes(x = a, color = `Arch(m)`)) +
  geom_density() +
  theme_bw() +
  # labs(
  #   title = TeX(r"(Posterior densities of $\alpha_1$)"),
  #   y = paste0("P(alpha[", i, "])"),
  #   x =  paste0("alpha[", i, "]"),
  # )
  labs(
    title = TeX(r"(Posterior densities of $\alpha_{1,tr}$)"),
    y = TeX(r"($P(\alpha_{1,tr}|x)$)"),
    x =  TeX(r"($\alpha_{1,tr}$)"),
  )

ggsave(filename = "./img/googl/arch/posterior_alpha1tr.png",
       width = 1920, height = 1080, units = "px")

map(i:8, \(x) {
  tibble(
    arch = x,
    a = as.numeric(rstan::extract(models[[x]], pars = "alpha_tr")$alpha_tr[, i])
  )
}) |>
  bind_rows() |>
  group_by(arch) |>
  summarise(
    Average = round(mean(a), 2),
    SD = round(sd(a), 2),
    CI_lower = quantile(a, (1-alpha)/2),
    CI_upper = quantile(a, (1+alpha)/2)
  ) |>
  mutate(
    arch = paste("m =", arch),
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -arch,
    names_to = "Arch(m)"
  ) |>
  pivot_wider(
    names_from = "arch"
  ) |>
  kable(caption = paste0("GOOGL/ARCH: Posterio for $\alpha_{1, \text{tr}}$"))

map(i:8, \(x) {
  tibble(
    arch = x,
    a = as.numeric(rstan::extract(models[[x]], pars = "alpha")$alpha[, i])
  )
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch))
  ) |>
  ggplot(aes(x = a, color = `Arch(m)`)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\alpha_1$)"),
    y = TeX(r"($P(\alpha_1|x)$)"),
    x =  TeX(r"($\alpha_1$)"),
  )

ggsave(filename = "./img/googl/arch/posterior_alpha1.png",
       width = 1920, height = 1080, units = "px")

# ------------------------------------------------------------------------------
# Time series
# ------------------------------------------------------------------------------

map(1:8, \(x) {
  tibble(
    index = index(ret)[-seq_len(x)],
    arch = x,
    v = colMeans(rstan::extract(models[[x]], pars = "sigma")$sigma)
  )
}) |>
  bind_rows() |>
  mutate(arch = factor(arch)) |>
  ggplot(aes(x = index, y = v, color = arch)) +
  geom_line() +
  facet_wrap(~arch) +
  theme_bw() +
  labs(
    title = "Comparison of estimated volatility",
    x = NULL,
    y = TeX(r"($\sigma$)")
  )


ggsave(filename = "./img/googl/arch/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

map(1:8, \(x) {
  tibble(
    Index = index(ret)[-seq_len(x)],
    arch = x,
    v = colMeans(rstan::extract(models[[x]], pars = "sigma")$sigma)
  )
}) -> x

x |>
  bind_rows() |>
  left_join(as_tibble(fortify(ret)), by = "Index") |>
  filter(arch == 1) |>
  select(-arch) |>
  pivot_longer(
    cols = -Index
  ) |>
  ggplot(aes(x = Index, y = value)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = "free_y") +
  theme_bw()

x |>
  bind_rows() |>
  left_join(as_tibble(fortify(ret)), by = "Index") |>
  filter(arch == 1) |>
  mutate(
    v = v**2,
    GOOGL.Open = GOOGL.Open ** 2
  ) |>
  ggplot(aes(x = v, y = GOOGL.Open)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Variance (ARCH model)",
    y = "Realized Variance (Squared Log Returns)",
    title = "ARCH(8) Model Evaluation"
  ) +
  theme_minimal()
