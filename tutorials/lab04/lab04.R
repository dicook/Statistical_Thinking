## ---- lib
library(ggplot2)
library(dplyr)


## ---- ex-plot
df <- data.frame(x = seq(0, 4, 0.008),  
                 d = dnorm(seq(0, 4, 0.008), mean = 2, sd = 0.5))
sub1 <- df %>% filter(x <= 1.3)
sub2 <- df %>% filter(x >= 1.9)
ggplot() +
  geom_line(data = df, aes(x = x, y = d)) +
  geom_vline(xintercept = 1.3, col = "#b2182b") +
  geom_ribbon(data = sub1, aes(x, ymin = 0, ymax = d), fill = "#b2182b",
              alpha = 0.7) +
  geom_text(inherit.aes = FALSE, aes(x = 0.5, y = 0.4, label = "Pr(x < 1.3)"),
            size = 8, col = "#b2182b") +
  geom_vline(xintercept = 1.9, col = "#2166ac") +
  geom_ribbon(data = sub2, aes(x, ymin = 0, ymax = d), fill = "#2166ac",
              alpha = 0.7) +
  geom_text(aes(x = 3.5, y = 0.4, label = "Pr(x > 1.9)"),
            size = 8, col = "#2166ac", inherit.aes = FALSE)

## ---- ex-prob1
pnorm(1.3, mean = 2, sd = 0.5)
## ---- ex-prob2
pnorm(1.9, mean = 2, sd = 0.5, lower.tail = FALSE)
1 - pnorm(1.9, mean = 2, sd = 0.5)
## ---- ex-prob3
pnorm(2.2, mean = 2, sd = 0.5) - pnorm(1.8, mean = 2, sd = 0.5)

## ---- ex-quantile
qnorm(c(0.53, 0.12, 0.84, 1.2), mean = -3, sd = 4)

## ---- ex-dens
dnorm(c(-1, -0.2, -2), mean = -1.2, sd = 0.8)

## ---- ex-weibull
library(ggplot2)
library(dplyr)
xgrid <- seq(0, 7, 0.01)
df_weibull <- data.frame(x = xgrid,
                         y1 = dweibull(xgrid, shape = 2, scale = 2.5),
                         y2 = dweibull(xgrid, shape = 1, scale = 2.5),
                         y3 = dweibull(xgrid, shape = 1, scale = 1))
df_weibull %>% 
  ggplot(aes(x = xgrid)) +
  geom_line(aes(y = y1), col = "red") +
  geom_line(aes(y = y2), col = "blue") +
  geom_line(aes(y = y3), col = "orange") +
  xlab("x") +
  ylab("densities")

## ---- q1-a
set.seed(123)
n <- 500
alpha <- 2
beta <- 4
df_gamma <- data.frame(xgamma = rgamma(n, shape = alpha, rate = beta))
## ---- q1-b
df_gamma$xq <- qgamma(c(1 - 0.5^(1/n), # i = 1
                        (2:(n-1) - 0.3175) / (n + 0.365), # i = 2, ... , n - 1
                        0.5^(1/n)), # i = n
                      alpha, beta) # theoretical quantiles
## ---- q1-c
df_gamma %>% 
  ggplot(aes(x = sort(xgamma), y = xq)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + # X = Y line
  xlab("Sample quantiles") +
  ylab("Theoretical quantiles") +
  coord_equal()

## ---- q2-df
set.seed(123)
X2 <- data.frame(x = rgamma(n = 267, 1.2, 0.25))
## ---- q2-hist
ggplot(X2, aes(x = x)) +
  geom_histogram(binwidth = 2)

## ---- q2-nmle
nmle <- function(x, alpha, beta) {
  # L(alpha, beta) = product of joint density functions
}

## ---- q2-nmle-fun
nmle <- function(x, alpha, beta) {
  likelihood <- prod(dgamma(x, alpha, beta))
  return(likelihood)
}


## ---- q2-plot1
alpha <- seq(0.9, 1.5, 0.01)
beta <- seq(0.18, 0.32, 0.005)
g <- expand.grid(x = alpha, y = beta) # all possible combinations of alpha and beta
g$l <- 0
for (i in 1:nrow(g)) {
  g$l[i] <- nmle(X2$x, g$x[i], g$y[i])
}
## ---- q2-plot2
ggplot(g, aes(x = x, y = y, fill = l)) + 
  geom_tile() + 
  scale_fill_continuous("L") +
  xlab(expression(alpha)) + 
  ylab(expression(beta)) + 
  theme_bw() + 
  theme(aspect.ratio = 1)

## ---- q2-plotly
set.seed(123)
x <- rgamma(n = 267, 1.2, 0.25)

nmle <- function(x, alpha, beta) {
  likelihood <- prod(dgamma(x, alpha, beta))
  return(likelihood)
}

alpha <- seq(0.9, 1.5, 0.01)
beta <- seq(0.18, 0.32, 0.005)
m <- matrix(NA, nrow = length(alpha), ncol = length(beta))
for (i in seq_along(alpha)) {
  for (j in seq_along(beta)) {
    m[i, j] <- nmle(x, alpha[i], beta[j])
  }
}

library(plotly)
library(htmlwidgets)
# there must be a precision issue with the colorbar...
p <- plot_ly(x = ~alpha, y = ~beta, z = ~10^200*m) %>% add_surface()
saveWidget(p, "3dscatter.html")

## ---- q2-fit
library(MASS)
fitdistr(X2$x, "gamma")

## ---- q3
# install.packages("xts")
# install.packages("sp")
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type = "source")
library(CASdatasets)
data(danishuni)
