library(quantmod)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
  library(knitr)
  library(latex2exp)
library(rstan)
  library(patchwork)
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
png("./img/googl/closing.png", width = 1920, height = 1080, res = 200)
plot(data)
dev.off()

# Get and plot returns
ret <- log(data$GOOGL.Open/lag(data$GOOGL.Open))[-1]
png("./img/googl/logret.png", width = 1920, height = 1080, res = 200)
plot(ret, ylim = c(-0.09, 0.16))
points(ret[which.min(ret)], col = "red", pch = 4, lwd=2)
points(ret[which.max(ret)], col = "green", pch = 4, lwd=2)
dev.off()

# Compute statistics for returns
ret |>
  fortify() |>
  as_tibble() |>
  mutate(x = GOOGL.Open) |>
  summarise(
    Start = min(Index),
    End = max(Index),
    n = n(),
    Average = mean(x),
    Variance  = var(x),
    "St. dev."   = sd(x),
    Min  = min(x),
    Max  = max(x)
  ) |>
  mutate(across(
    .cols = where(is.double),
    .fns = \(x) sprintf("%.2f", x)
  )) |>
  kable(caption = "Description of log returns of GOOGL")

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

get_model <- function(m, data) {
  data$m <- m

  print(paste("Modeling", m))
  stan(
    file = "./stan/models/ARCH.stan",
    model_name = "ARCH",
    data = data,
    seed = m,
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
models <- map(1:8, \(m) get_model(m = m, data = data))
print(Sys.time() - a)
saveRDS(models, file = "./stan/data/ARCH.RDS")
# models <- readRDS("./stan/data/ARCH.RDS")
# ------------------------------------------------------------------------------
# Model statistics
# ------------------------------------------------------------------------------
map(1:8, \(x) {
  as_tibble(
    summary(models[[x]], pars = c("nu", "alpha", "alpha0"))$summary,
    rownames = "param"
  ) |>
    mutate(m = x)
}) |>
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
    Average = mean(nu),
    SD = sd(nu),
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
map(1:8, \(x) {
  tibble(
    arch = x,
    a0 = as.numeric(rstan::extract(models[[x]], pars = "alpha0")$alpha0)
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
map(1:8, \(x) {
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
i <- 8
tibble(
  Index = index(ret)[-seq_len(i)],
  true = ret[-seq_len(i)],
  l = apply(extract(models[[i]], pars = "y_pred")$y_pred, 2, \(x) quantile(x, 0.055)),
  u = apply(extract(models[[i]], pars = "y_pred")$y_pred, 2, \(x) quantile(x, 0.945)),
) |>
  ggplot(aes(x = Index)) +
  geom_line(aes(y = true), color = "green") +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.4) +
  theme_bw()

data <-
  map(1:8, \(i) {
    tibble(
      Index = index(ret)[-seq_len(i)],
      true = coredata(ret[-seq_len(i)]),
      l = apply(
        extract(models[[i]], pars = "y_pred")$y_pred,
        2,
        \(x) quantile(x, 0.055)
      ),
      u = apply(
        extract(models[[i]], pars = "y_pred")$y_pred,
        2,
        \(x) quantile(x, 0.945)
      )
    ) |>
      mutate(
        model = paste0("ARCH(", i, ")")
      )
  }) |>
  bind_rows()

data |>
  ggplot(aes(x = Index)) +
  geom_line(aes(y = true), color = "green") +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.6) +
  theme_bw() +
  facet_wrap(vars(model), nrow = 2)

ggsave(filename = "./img/googl/arch/posterior_prediction.png",
       width = 1920, height = 1080, units = "px")
