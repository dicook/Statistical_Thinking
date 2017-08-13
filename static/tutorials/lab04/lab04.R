## ---- lib
library(ggplot2)
library(dplyr)

## ---- ex-plot
df <- data.frame(
  x = seq(-0.3, 7, 0.008),  
  d = dnorm(seq(-0.3, 7, 0.008), mean = 3.3, sd = 1.1)
)
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
  geom_text(aes(x = 2.7, y = 0.4, label = "Pr(x > 1.9)"),
            size = 8, col = "#2166ac", inherit.aes = FALSE)

## ---- ex-prob1
pnorm(1.3, mean = 3.3, sd = 1.1)
## ---- ex-prob2
pnorm(1.9, mean = 3.3, sd = 1.1, lower.tail = FALSE)
1 - pnorm(1.9, mean = 3.3, sd = 1.1)
## ---- ex-prob3
pnorm(2.2, mean = 3.3, sd = 1.1) - pnorm(1.8, mean = 3.3, sd = 1.1)

## ---- ex-quantile
qnorm(c(0.53, 0.12, 0.84, 1.2), mean = -10, sd = 4)

## ---- ex-dens
dnorm(c(13, 4, 20), mean = 12, sd = 5)

## ---- ex-weibull
library(ggplot2)
xgrid <- seq(0, 6, 0.01)
df_weibull <- data.frame(
  x = xgrid,
  y1 = dweibull(xgrid, shape = 3, scale = 1.5),
  y2 = dweibull(xgrid, shape = 2, scale = 2),
  y3 = dweibull(xgrid, shape = 1, scale = 1)
)
ggplot(data = df_weibull, aes(x = xgrid)) +
  geom_line(aes(y = y1), colour = "red") +
  geom_line(aes(y = y2), colour = "blue") +
  geom_line(aes(y = y3), colour = "orange") +
  xlab("x") +
  ylab("densities")

## ---- q1-a
set.seed(123)
n <- 500
alpha <- 4
beta <- 2
df_gamma <- data.frame(
  xgamma = rgamma(n, shape = alpha, rate = beta)
)
head(df_gamma)
## ---- q1-b
df_gamma$xq <- qgamma(
  c(1 - 0.5^(1/n), # i = 1
  (2:(n-1) - 0.3175) / (n + 0.365), # i = 2, ... , n - 1
  0.5^(1/n)), # i = n
  alpha, beta
) # theoretical quantiles
head(df_gamma)
## ---- q1-c
ggplot(data = df_gamma, aes(x = sort(xgamma), y = xq)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + # X = Y line
  xlab("Sample quantiles") +
  ylab("Theoretical quantiles") +
  coord_equal()

## ---- q2-df
set.seed(123)
X2 <- data.frame(x = rgamma(n = 544, 3.2, 1.7))
## ---- q2-hist
ggplot(X2, aes(x = x)) +
  geom_histogram(binwidth = 0.3)

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
alpha <- seq(2.7, 3.8, 0.01)
beta <- seq(1.45, 2.2, 0.01)
# all possible combinations of alpha and beta
g <- expand.grid(x = alpha, y = beta) 
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
m <- matrix(g$l, nrow = length(alpha))
library(plotly)
plot_ly(x = ~ beta, y = ~ alpha, z = m) %>% 
  add_surface()

## ---- q2-fit
library(MASS)
fitdistr(X2$x, "gamma")

## ---- q3
# install.packages("xts")
# install.packages("sp")
# install.packages("CASdatasets", 
#   repos = "http://cas.uqam.ca/pub/R/", type = "source")
library(CASdatasets)
data(usworkcomp)
