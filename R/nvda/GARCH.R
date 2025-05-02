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

get_model <- function(p, q, name, data) {
  print(paste("Modeling", name))
  data$p <- p
  data$q <- q

  stan(
    file = "./stan/ARCH.stan",
    model_name = "GARCH",
    data = data,
    seed = p,
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
models <-
  expand.grid(
    p = 5,
    q = 1:8
  ) |>
  mutate(
    name = paste0("GARCH(", p, ", ", q, ")")
  ) |>
  rowwise() |>
  mutate(
    model = list(
      get_model(p = p, q = q, name = name, data = data)
    )
  )
print(Sys.time() - a)
# saveRDS(models, file = "./R/nvda/data/GARCH.RDS")
# models <- readRDS("./R/nvda/data/GARCH.RDS")

# Model statistics -------------------------------------------------------------
pmap(models, \(p, name, model) {
  as_tibble(
    summary(model, pars = c("nu", "alpha", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(name = name) |>
    filter(param != paste0("alpha[", p + 1, "]"))
}) |>
  bind_rows() |>
  select(name, param, Rhat) |>
  mutate(
    param = ordered(
      param,
      levels = c("nu", "alpha0", paste0("alpha[", models$p, "]")))
  ) |>
  pivot_wider(
    names_from = name,
    values_from = Rhat
  ) |>
  arrange(param) |>
  kable(caption = "NVDA/ARCH: Shrinkage factors")

pmap(models, \(p, name, model) {
  as_tibble(
    summary(model, pars = c("nu", "alpha", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(name = name) |>
    filter(param != paste0("alpha[", p + 1, "]"))
}) |>
  bind_rows() |>
  select(name, param, n_eff) |>
  mutate(
    param = ordered(param,
                    levels = c("nu", "alpha0",
                               paste0("alpha[", models$p, "]")))

  ) |>
  pivot_wider(
    names_from = name,
    values_from = n_eff
  ) |>
  arrange(param) |>
  kable(caption = "NVDA/ARCH: ESS")

# Compare degrees of freedom ---------------------------------------------------
pmap(models, \(p, name, model) {
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

ggsave(filename = "./img/nvda/arch/posterior_nu.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, name, model) {
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
  kable(caption = "NVDA/ARCH: Tabular description of posterior for $\nu$")

# Compare intercept ------------------------------------------------------------
pmap(models, \(p, name, model) {
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

ggsave(filename = "./img/nvda/arch/posterior_alpha0.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, name, model) {
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
  kable(caption = "NVDA/ARCH: Tabular description of posterior for $\alpha_0$")

# Compare alpha1 ---------------------------------------------------------------
pmap(models, \(p, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "alpha")$alpha[, 1]
  )
}) |>
  bind_rows() |>
  ggplot(aes(x = x, color = Model)) +
  geom_density() +
  theme_bw() +
  labs(
    title = TeX(r"(Posterior densities of $\alpha_1$)"),
    y = TeX(r"($P(\alpha_1|x)$)"),
    x =  TeX(r"($\alpha_1$)"),
  )

ggsave(filename = "./img/nvda/arch/posterior_alpha1.png",
       width = 1920, height = 1080, units = "px")

pmap(models, \(p, name, model) {
  tibble(
    Model = name,
    x = extract(model, pars = "alpha")$alpha[, 1]
  )
}) |>
  bind_rows() |>
  group_by(Model) |>
  summarise(
    Average = round(mean(x), 2),
    SD = round(sd(x), 2),
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
  kable(caption = paste0("NVDA/ARCH: Posterio for $\alpha_1$"))

# All parameters ---------------------------------------------------------------
map(models$p, \(i) {
  models[seq(from = i, to = nrow(models)), ] |>
    pmap(\(p, name, model) {
      tibble(
        coeff = i,
        Model = name,
        x = extract(model, pars = "alpha")$alpha[, i]
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
    title = TeX(r"(Posterior densities of constrained parameters $\alpha_i$)"),
    y = TeX(r"($P(\alpha_i | x)$)"),
    x =  TeX(r"($\alpha_i$)")
  )

ggsave(filename = "./img/nvda/arch/posterior_alpha_i.png",
       width = 1920, height = 1080, units = "px")

map(models$p, \(i) {
  models[seq(from = i, to = nrow(models)), ] |>
    pmap(\(p, name, model) {
      tibble(
        coeff = i,
        Model = name,
        x = extract(model, pars = "alpha")$alpha[, i]
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
  kable(caption = r"(NVDA/ARCH: Expected $\alpha_i$ for all models)")

# Volatility -------------------------------------------------------------------
pmap(models, \(p, name, model) {
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

ggsave(filename = "./img/nvda/arch/posterior_volatility.png",
       width = 1920, height = 1080, units = "px")

# Total ------------------------------------------------------------------------
pmap(models, \(p, name, model) {
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

ggsave(filename = "./img/nvda/arch/posterior_sum.png",
       width = 1920, height = 1080, units = "px")

# Prediction -------------------------------------------------------------------
df_pred <-
  pmap(models, \(p, name, model) {
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

ggsave(filename = "./img/nvda/arch/posterior_prediction.png",
       width = 1920, height = 1080, units = "px")

# Comparison -------------------------------------------------------------------
pmap(models, \(p, name, model) {
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
  kable(caption = r"(NVDA/ARCH: Comparison of models using ELPD.)")

# Description ------------------------------------------------------------------
models |>
  filter(Model == "ARCH(5)") |>
  ( \(x) {
    extract(x$model[[1]], pars = c(
      "nu", "alpha0", paste0("alpha[", seq_len(x$p), "]")
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
  kable(caption = "NVDA/ARCH: Description of the final model.")

df_pred |>
  filter(Model == "ARCH(5)") |>
  ggplot(aes(x = Index)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.7) +
  geom_line(aes(y = true)) +
  theme_bw() +
  labs(
    title = TeX(r"(Most optimal model: ARCH(5))"),
    subtitle = TeX(r"(with shaded 89% percentile interval)"),
    x = NULL, y = TeX(r"($p$)")
  )

ggsave(filename = "./img/nvda/arch/posterior_final.png",
       width = 1920, height = 1080, units = "px")
