library(quantmod)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(latex2exp)
library(rstan)
library(stringr)
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

# Generate models
y <-
  coredata(ret) |>
  as.vector()

get_model <- function(data) {
  print("Modeling SV")
  stan(
    file = "./stan/models/SV.stan",
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
models <- get_model(data = data)
print(Sys.time() - a)
saveRDS(models, file = "./stan/data/SV.RDS")
# ------------------------------------------------------------------------------
# Model statistics
# ------------------------------------------------------------------------------
map(1:2, \(x) {
  as_tibble(
    summary(models[[x]])$summary,
    rownames = "param"
  ) |>
    filter(str_detect(param, "sigma", TRUE)) |>
    filter(str_detect(param, "coefs", TRUE)) |>
    filter(str_detect(param, "lp__", TRUE)) |>
    mutate(m = x)
}) |>
  bind_rows() |>
  select(m, param, Rhat) |>
  pivot_wider(
    names_from = m,
    values_from = Rhat
  ) |>
  arrange(param) |>
  kable(caption = "GOOGL/GARCH: Shrinkage factors")

map(1:2, \(x) {
  as_tibble(
    summary(models[[x]])$summary,
    rownames = "param"
  ) |>
    filter(str_detect(param, "sigma", TRUE)) |>
    filter(str_detect(param, "coefs", TRUE)) |>
    filter(str_detect(param, "lp__", TRUE)) |>
    mutate(m = x)
}) |>
  bind_rows() |>
  select(m, param, n_eff) |>
  pivot_wider(
    names_from = m,
    values_from = n_eff
  ) |>
  arrange(param) |>
  kable(caption = "GOOGL/GARCH: ESS")

# ------------------------------------------------------------------------------
# Compare degrees of freedom
# ------------------------------------------------------------------------------
map(1:2, \(x) {
  tibble(
    garch = x,
    nu =extract(models[[x]], pars = "nu")$nu
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = nu, color = factor(garch))) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\nu$)"),
    y = TeX(r"(P(\nu|x))"),
    x = TeX(r"(\nu)")
  )

ggsave(filename = "./img/googl/garch/posterior_nu.png",
       width = 1920, height = 1080, units = "px")

alpha <- 0.89
map(1:2, \(x) {
  tibble(
    arch = x,
    nu = extract(models[[x]], pars = "nu")$nu
  )
}) |>
  bind_rows() |>
  group_by(arch) |>
  summarise(
    Average = mean(nu),
    SD = sd(nu),
    CI_lower = quantile(nu, (1-alpha)/2),
    CI_upper = quantile(nu, (1+alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -arch,
    names_to = "GARCH(m)"
  ) |>
  pivot_wider(
    names_from = "arch"
  ) |>
  kable(caption = "GOOGL/ARCH: Tabular description of posterior for $\nu$")

# ------------------------------------------------------------------------------
# Compare intercept
# ------------------------------------------------------------------------------
map(1:2, \(x) {
  tibble(
    arch = x,
    a0 = extract(models[[x]], pars = "alpha0")$alpha0
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = a0, color = factor(arch))) +
  geom_density() +
  scale_x_continuous(labels = scales::label_number()) +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\alpha_0$)"),
    y = TeX(r"($P(\alpha_0|x)$)"),
    x = TeX(r"($\alpha_0$)")
  )

ggsave(filename = "./img/googl/arch/posterior_alpha0.png",
       width = 1920, height = 1080, units = "px")

alpha <- 0.89
map(1:2, \(x) {
  tibble(
    arch = x,
    a0 = extract(models[[x]], pars = "alpha0")$alpha0
  )
}) |>
  bind_rows() |>
  group_by(arch) |>
  summarise(
    Average = mean(a0),
    SD = sd(a0),
    CI_lower = quantile(a0, (1-alpha)/4),
    CI_upper = quantile(a0, (1+alpha)/4)
  ) |>
  mutate(
    arch = paste("m =", arch),
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.4f", x)
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
    a = as.numeric(rstan::extract(models[[x]], pars = "alpha")$alpha[, i])
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
  kable(caption = paste0("GOOGL/ARCH: Posterio for $\alpha{1, \text{tr}}$"))

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
# All parameters
# ------------------------------------------------------------------------------
map(1:8, \(i) {
  map(i:8, \(x) {
    tibble(
      coeff = i,
      arch = x,
      a = as.numeric(rstan::extract(models[[x]], pars = "alpha")$alpha[, i])
    )
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch)),
    coeff = factor(paste("i =", coeff))
  ) |>
  ggplot(aes(x = a, color = `Arch(m)`)) +
  facet_wrap(vars(`coeff`), scales = "free") +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of constrained parameters $\alpha_{i, tr}$)"),
    y = TeX(r"($P(\alpha_{i, tr} | x)$)"),
    x =  TeX(r"($\alpha_{i, tr}$)")
  )

ggsave(filename = "./img/googl/arch/posterior_alpha_i.png",
       width = 1920, height = 1080, units = "px")

map(1:8, \(i) {
  map(i:8, \(x) {
    tibble(
      coeff = i,
      arch = x,
      a = as.numeric(rstan::extract(models[[x]], pars = "alpha")$alpha[, i])
    )
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste("m =", arch)),
    coeff = factor(paste("i =", coeff))
  ) |>
  group_by(coeff, `Arch(m)`) |>
  summarise(x = sprintf("%.2f", mean(a)),
            .groups = "drop") |>
  pivot_wider(
    names_from = `Arch(m)`,
    values_from = x,
    values_fill =  "-"
  ) |>
  kable(caption = r"(GOOGL/ARCH: Expected $\alpha_{i, \text{tr}$ for all models)")

# ---------------------------------------------------------------------------------------
# Time series
# ------------------------------------------------------------------------------
map(1, \(x) {
  tibble(
    index = index(ret)[-seq_len(x)],
    arch = x,
    v = colMeans(sqrt(rstan::extract(models[[x]], pars = "sigma2")$sigma2))
  )
}) |>
  bind_rows() |>
  mutate(
    `Arch(m)` = factor(paste0("ARCH(", arch, ")"))
  ) |>
  ggplot(aes(x = index, y = v, color = `Arch(m)`)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~`Arch(m)`) +
  theme_bw() +
  labs(
    title = "Comparison of estimated volatility",
    x = NULL,
    y = TeX(r"($\sigma$)")
  )


ggsave(filename = "./img/googl/arch/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

# ---------------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------
i <- 1
tibble(
  Index = index(ret),
  true = ret,
  l = apply(extract(models[[i]], pars = "y_pred")$y_pred, 2, \(x) quantile(x, 0.055)),
  u = apply(extract(models[[i]], pars = "y_pred")$y_pred, 2, \(x) quantile(x, 0.945)),
) |>
  ggplot(aes(x = Index)) +
  geom_line(aes(y = true), color = "green") +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.4) +
  theme_bw()
