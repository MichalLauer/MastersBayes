library(latex2exp)

# Posterior density
p <- function(theta, data) {
  prior <- dbeta(theta, shape1 = 1, shape2 = 1)
  likelihood <- dbinom(sum(data), size = 25, prob = theta)

  return(prior * likelihood)
}

plot_posterior <- function(seed, prob) {
  set.seed(seed)
  # Observed data
  observed_data <- rbinom(25, size = 1, prob = prob)
  # Number of iterations
  number_of_iterations <- 10000
  # Generated samples
  samples <- numeric(number_of_iterations + 1)
  samples[1] <- 0.5

  for (i in seq_len(number_of_iterations)) {
    proposal <- rnorm(1, mean = samples[i], sd = 1)
    proposal <- max(min(1, proposal), 0)
    ratio <- p(proposal, observed_data) / p(samples[i], observed_data)
    if (ratio >= 1 || runif(1) <= ratio) {
      samples[i + 1] <- proposal
    } else {
      samples[i + 1] <- samples[i]
    }
  }

  x_vals <- seq(0, 1, by = 0.01)
  n <- length(observed_data)
  k <- sum(observed_data)
  beta_density <- dbeta(x_vals, k, n - k)

  # Plot histogram
  hist(samples,
       breaks = 20,
       probability = TRUE,
       col = rgb(0.2, 0.2, 0.8, 0.4),
       border = "white",
       xlim = c(0, 1),
       main = paste0("Prob = ", prob),
       xlab = TeX(r"($\theta$)"),
       ylab = TeX(r"($p(\theta|x)$)"))

  # Overlay density
  lines(x_vals, beta_density, col = "darkred", lwd = 2)
}
png("./img/sim_ma.png", width = 1920, height = 1080, res = 100)
layout_matrix <- rbind(c(1, 2, 3),
                       c(4, 4, 4))  # Legend spans all 3 columns
layout(layout_matrix, heights = c(4, .5))

# Plot margins
par(mar = c(4, 4, 2, 1), cex.main = 2.5, cex.lab = 2, cex.axis = 1.8)

# Three side-by-side plots
plot_posterior(1, 0.2)
plot_posterior(1, 0.5)
plot_posterior(1, 0.8)

# Plot just the legend
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Generated samples", "True posterior distribution"),
       pch = c(15, NA),
       col = c(rgb(0.2, 0.2, 0.8, 0.7), "darkred"),
       lty = c(NA, 1),
       lwd = c(NA, 2),
       pt.cex = 5,
       cex = 2.5,  # Increase legend text size
       horiz = TRUE,
       bty = "n")
dev.off()
