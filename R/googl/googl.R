# library(quantmod)
# library(purrr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(knitr)
# library(latex2exp)
# library(rstan)
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

# Prior for Degrees of Freedom
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
                    labels = c("density" = "89% credible interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about $\nu$)"),
    subtitle = TeX(paste0(
      "$\\nu \\sim \\Gamma(\\alpha = ", sh, ", \\beta = ", sc, ")$")),
    x = TeX(r"($\nu$)"),
    y = TeX(r"(P($\nu$))")
  )
ggsave(filename = "./img/googl/arch/apriori_nu.png",
       width = 1920, height = 1080, units = "px")


# Prior for alpha
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
                    labels = c("density" = "89% credible interval")) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = TeX(r"(Prior knowledge about $\alpha_i$)"),
    subtitle = TeX(paste0(
      "$\\alpha_i \\sim \\Gamma(\\alpha = ", sh, ", \\beta = ", sc, ")$")),
    x = TeX(r"($\alpha_i $)"),
    y = TeX(r"(P($\alpha_i $))")
  )
ggsave(filename = "./img/googl/arch/apriori_alpha.png",
       width = 1920, height = 1080, units = "px")
