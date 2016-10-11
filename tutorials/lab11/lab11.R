## ---- q1-temp
library(dplyr)
library(ggplot2)
set.seed(1986)
r <- 1 # diameter 1
n <- c(100, 1000, 10000, 100000)
pi_approx <- pi_estimate <- numeric(length = length(n))
for (i in seq_along(n)) {
  x <- # randomly generated from a uniform distribution
  y <- # randomly generated from a uniform distribution
  is_inside <- # whether (x, y) falls in the circle
  pi_estimate[i] <- # estimate pi
  pi_approx[i] <- # compare pi with pi_estimate
}


## ---- q1
library(dplyr)
library(ggplot2)
set.seed(1986)
r <- 1
n <- c(100, 1000, 10000, 100000)
pi_approx <- pi_estimate <- numeric(length = length(n))
for (i in seq_along(n)) {
  x <- runif(n[i], min = -r, max = r)
  y <- runif(n[i], min = -r, max = r)
  is_inside <- (x^2 + y^2) <= r^2
  pi_estimate[i] <- 4 * sum(is_inside) / n[i]
  pi_approx[i] <- abs(pi_estimate[i] - pi)
}

## ---- q1-plot
# n = 100000
pi_df <- data.frame(x = x, y = y, is_inside = is_inside)
ggplot(aes(x = x, y = y, colour = is_inside), data = pi_df) +
geom_point() +
coord_equal()

## ---- q2-temp
f <- function(x) x * (1 - x) * exp(x) / (3 - exp(1)) 
x <- seq(0, 1, 0.01)

rdist <- function(c) { # choose c with f(x)<=c for all x
  x <- runif(1,0,1) # step 1
  u <- runif(1,0,1) # step 2
  output <- ifelse(c * u <= f(x), x, -1)
  return(output)
}

const <- c(1.56, 2, 5, 20)
rejected_prop <- numeric(length = length(const))
accepted_df <- vector(mode = "list", length = length(const))
for (i in seq_along(const)) {
  ## fill in the necessary code
}

## ---- q2
f <- function(x) x * (1 - x) * exp(x) / (3 - exp(1)) 
x <- seq(0, 1, 0.01)

rdist <- function(c) { # choose c with f(x)<=c for all x
  x <- runif(1, 0, 1) # step 1
  u <- runif(1, 0, 1) # step 2
  output <- ifelse(c * u <= f(x), x, -1)
  return(output)
}

const <- c(1.56, 2, 5, 20)
rejected_prop <- numeric(length = length(const))
accepted_df <- vector(mode = "list", length = length(const))
for (i in seq_along(const)) {
  samples <- replicate(1000, rdist(const[i]))
  rejected_samples <- samples == -1
  rejected_prop[i] <- sum(rejected_samples) / 1000
  accepted_df[[i]] <- data.frame(x = samples[!rejected_samples], const = const[i])
}
accepted_df <- bind_rows(accepted_df)
actual_dens <- data.frame(x = x, y = f(x), const = rep(const, each = length(x)))
ggplot() +
  geom_histogram(aes(x = x, y = ..density..), data = accepted_df, bins = 10) +
  facet_grid(const ~ ., scales = "free_y") +
  geom_line(aes(x = x, y = y), data = actual_dens, color = "red") +
  facet_wrap(~ const, scales = "free_y", ncol = 2)

## ---- q3
f <- function(x) (1 / pi) * (1 / (1 + x^2))

for (b in c(0.1, 1, 10)) {
  x <- 0
  N <- 1000
  samples <- numeric(N)
  for (i in seq(N)) {
    y <- rnorm(1, mean = 0, sd = b)
    r <- min(f(y)/f(x), 1)
    
    U <- runif(1)
    if (U < r) {
      x <- y
    }
    samples[i] <- x
  }
  xgrid <- seq(min(samples), max(samples), by = 0.01)
  # samples_df <- data.frame(x = samples, index = 1:length(samples))
  # actual_dens <- data.frame(x = xgrid, y = f(xgrid))
  # ggplot() +
  #   geom_histogram(aes(x = x, y = ..density..), data = samples_df) +
  #   geom_line(aes(x = xgrid, y = y), data = actual_dens)
  # ggplot(aes(x = index, y = x), data = samples_df) +
  #   geom_line()
}

samples_df <- data.frame(x = samples, index = 1:length(samples))
actual_dens <- data.frame(x = xgrid, y = f(xgrid))
ggplot() +
  geom_histogram(aes(x = x, y = ..density..), data = samples_df) +
  geom_line(aes(x = xgrid, y = y), data = actual_dens, color = "red")
ggplot(aes(x = index, y = x), data = samples_df) +
  geom_line()
