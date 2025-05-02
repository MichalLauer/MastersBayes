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

# get_model <- function(p, q, name, data) {
#   print(paste("Modeling", name))
#   data$p <- p
#   data$q <- q
#
#   stan(
#     file = "./stan/GARCH.stan",
#     model_name = "GARCH",
#     data = data,
#     seed = as.numeric(paste0(p, q)),
#     iter = 8000,
#     warmup = 2000,
#     cores = parallel::detectCores() - 1
#   )
# }

# data <- list(
#   T = length(y),
#   y = y
# )

# a <- Sys.time()
# models <-
#   expand.grid(
#     p = 5,
#     q = 1:8
#   ) |>
#   mutate(
#     Model = paste0("GARCH(", p, ", ", q, ")")
#   ) |>
#   rowwise() |>
#   mutate(
#     model = list(
#       get_model(p = p, q = q, name = name, data = data)
#     )
#   )
# print(Sys.time() - a)
# saveRDS(models, file = "./R/nvda/data/GARCH.RDS")
# models <- readRDS("./R/nvda/data/GARCH.RDS")

# Model statistics -------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  as_tibble(
    summary(model, pars = c("nu", "alpha", "beta", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(Model = name)
}) |>
  bind_rows() |>
  select(Model, param, Rhat) |>
  mutate(
    param = ordered(
      param,
      levels = c("nu", "alpha0",
                 paste0("alpha[", seq_len(unique(models$p)), "]"),
                 paste0("beta[", models$q, "]")
      )
    )
  ) |>
  pivot_wider(
    names_from = Model,
    values_from = Rhat
  ) |>
  arrange(param) |>
  kable(caption = "NVDA/GARCH: Shrinkage factors")

pmap(models, \(p, q, name, model) {
  as_tibble(
    summary(model, pars = c("nu", "alpha", "beta", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(Model = name)
}) |>
  bind_rows() |>
  select(Model, param, n_eff) |>
  mutate(
    param = ordered(
      param,
      levels = c("nu", "alpha0",
                 paste0("alpha[", seq_len(unique(models$p)), "]"),
                 paste0("beta[", models$q, "]")
      )
    )
  ) |>
  pivot_wider(
    names_from = name,
    values_from = n_eff
  ) |>
  arrange(param) |>
  kable(caption = "NVDA/GARCH: ESS")

# Compare degrees of freedom ---------------------------------------------------
pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "nu")$nu
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = x, color = Model)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\nu$)"),
    y = TeX(r"(P(\nu|x))"),
    x = TeX(r"(\nu)")
  )

ggsave(filename = "./img/nvda/garch/posterior_nu.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "nu")$nu
  )
}) |>
  bind_rows() |>
  group_by(Model) |>
  summarise(
    Average = mean(x),
    SD = sd(x),
    CI_lower = quantile(x, (1-alpha)/2),
    CI_upper = quantile(x, (1+alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -Model,
    names_to = "Statistics"
  ) |>
  pivot_wider(
    names_from = Model
  ) |>
  kable(caption = "NVDA/GARCH: Tabular description of posterior for $\nu$")

# Compare intercept ------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "alpha0")$alpha0
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = x, color = Model)) +
  geom_density() +
  scale_x_continuous(labels = scales::label_number()) +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\alpha_0$)"),
    y = TeX(r"($P(\alpha_0|x)$)"),
    x = TeX(r"($\alpha_0$)")
  )

ggsave(filename = "./img/nvda/garch/posterior_alpha0.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "alpha0")$alpha0
  )
}) |>
  bind_rows() |>
  group_by(Model) |>
  summarise(
    Average = mean(x),
    SD = sd(x),
    CI_lower = quantile(x, (1-alpha)/4),
    CI_upper = quantile(x, (1+alpha)/4)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.4f", x)
    )
  ) |>
  pivot_longer(
    cols = -Model,
    names_to = "Statistics"
  ) |>
  pivot_wider(
    names_from = Model
  ) |>
  kable(caption = "NVDA/GARCH: Tabular description of posterior for $\alpha_0$")

# Compare beta1 ---------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "beta")$beta[, 1]
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = x, color = Model)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\beta_1$)"),
    y = TeX(r"($P(\beta_1|x)$)"),
    x =  TeX(r"($\beta_1$)"),
  )

ggsave(filename = "./img/nvda/garch/posterior_beta1.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "beta")$beta[, 1]
  )
}) |>
  bind_rows() |>
  group_by(Model) |>
  summarise(
    Average = round(mean(x), 2),
    SD = round(sd(x), 2),
    CI_lower = quantile(x, (1 - alpha)/2),
    CI_upper = quantile(x, (1 + alpha)/2)
  ) |>
  mutate(
    across(
      .cols = where(is.double),
      .fns = \(x) sprintf("%.2f", x)
    )
  ) |>
  pivot_longer(
    cols = -Model,
    names_to = "Statistics"
  ) |>
  pivot_wider(
    names_from = Model
  ) |>
  kable(caption = paste0("NVDA/GARCH: Posterio for $\beta_1$"))

# All parameters ---------------------------------------------------------------
map(models$q, \(i) {
  models[seq(from = i, to = nrow(models)), ] |>
    pmap(\(p, q, name, model) {
      tibble(
        coeff = i,
        Model = name,
        x = extract(model, pars = "beta")$beta[, i]
      )
    }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(
    coeff = factor(paste("i =", coeff))
  ) |>
  ggplot(aes(x = x, color = Model)) +
  facet_wrap(vars(`coeff`), scales = "free") +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of constrained parameters $\beta_i$)"),
    y = TeX(r"($P(\beta_i | x)$)"),
    x =  TeX(r"($\beta_i$)")
  )

ggsave(filename = "./img/nvda/garch/posterior_beta_i.png",
       width = 1920, height = 1080, units = "px")

map(models$q, \(i) {
  models[seq(from = i, to = nrow(models)), ] |>
    pmap(\(p, q, name, model) {
      tibble(
        coeff = i,
        Model = name,
        x = extract(model, pars = "beta")$beta[, i]
      )
    }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(
    coeff = factor(paste("i =", coeff))
  ) |>
  group_by(coeff, Model) |>
  summarise(x = sprintf("%.2f", mean(x)),
            .groups = "drop") |>
  pivot_wider(
    names_from = Model,
    values_from = x,
    values_fill =  "-"
  ) |>
  kable(caption = r"(NVDA/GARCH: Expected $\beta_i$ for all models)")

# Volatility -------------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  tibble(
    index = index(ret)[-seq_len(p)],
    Model = name,
    x = colMeans(sqrt(extract(model, pars = "sigma2")$sigma))
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = index, y = x, color = Model)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Model) +
  theme_bw() +
  labs(
    title = "Comparison of estimated volatility",
    x = NULL,
    y = TeX(r"($\sigma$)")
  )

ggsave(filename = "./img/nvda/garch/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

# Total ------------------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "total")$total
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = x, color = Model)) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = TeX(r"(Sum of coefficients)"),
    y = TeX(r"($P(\sum|x)$)"),
    x =  TeX(r"(\sum$)"),
  )

ggsave(filename = "./img/nvda/garch/posterior_sum.png",
       width = 1920, height = 1080, units = "px")

# Prediction -------------------------------------------------------------------
df_pred <-
  pmap(models, \(p, q, name, model) {
    tibble(
      Index = index(ret)[-seq_len(p)],
      true = coredata(ret[-seq_len(p)]),
      Model = name,
      l = apply(
        extract(model, pars = "y_pred")$y_pred,
        2,
        \(x) quantile(x, 0.055)
      ),
      u = apply(
        extract(model, pars = "y_pred")$y_pred,
        2,
        \(x) quantile(x, 0.945)
      ),
    )
  }) |>
  bind_rows()

df_pred |>
  ggplot(aes(x = Index)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.7) +
  geom_line(aes(y = true)) +
  facet_wrap(vars(Model), nrow = 2) +
  theme_bw() +
  labs(
    title = TeX(r"(Comparison of posterior distributions for $\sigma$)"),
    subtitle = TeX(r"(with shaded 89% percentile interval)"),
    x = NULL, y = TeX(r"($\sigma$)")
  )

ggsave(filename = "./img/nvda/garch/posterior_prediction.png",
       width = 1920, height = 1080, units = "px")

# Comparison -------------------------------------------------------------------
pmap(models, \(p, q, name, model) {
  if (p != 8) {
    ll <- extract_log_lik(model)[, -seq_len(8 - p)]
  } else {
    ll <- extract_log_lik(model)
  }

  loo(ll)
}) |>
  setNames(models$name) |>
  loo_compare() |>
  as_tibble(rownames = "Model") |>
  select(1:3) |>
  kable(caption = r"(NVDA/GARCH: Comparison of models using ELPD.)")

# Description ------------------------------------------------------------------
models |>
  filter(name == "GARCH(5, 1)") |>
  ( \(x) {
    extract(x$model[[1]], pars = c(
      "nu", "alpha0",
      paste0("alpha[", seq_len(unique(models$p)), "]"),
      paste0("beta[", models$q, "]")
    ))
  })() |>
  as_tibble() |>
  pivot_longer(
    cols = everything(),
    names_to = "Parameter"
  ) |>
  group_by(Parameter) |>
  summarise(
    `E(X)` = mean(value),
    Median = median(value),
    PI_lower = quantile(value, probs = (1 - alpha) / 2),
    PI_upper = quantile(value, probs = (1 + alpha) / 2)
  ) |>
  mutate(across(
    .cols = where(is.numeric),
    .fns = \(x) sprintf("%.4f", x)
  )) |>
  kable(caption = "NVDA/GARCH: Description of the final model.")

df_pred |>
  filter(Model == "GARCH(5, 1)") |>
  ggplot(aes(x = Index)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.7) +
  geom_line(aes(y = true)) +
  theme_bw() +
  labs(
    title = TeX(r"(Most optimal model: GARCH(5))"),
    subtitle = TeX(r"(with shaded 89% percentile interval)"),
    x = NULL, y = TeX(r"($p$)")
  )

ggsave(filename = "./img/nvda/garch/posterior_final.png",
       width = 1920, height = 1080, units = "px")
