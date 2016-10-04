## ---- warmup
library(knitr)
library(tidyr)
library(dplyr)

## ---- q2-temp
set.seed(1986)
theta <- 2
sigma_0 <- 3

alln <- c(1, 2, 5, 10, 100, 10000)
for(case in c(1, 2)){
  if (case == 1) {
    prior_mu <- 2
    prior_tau <- 2
  } else if (case == 2) {
    prior_mu <- 5
    prior_tau <- 1
  }
  
  for (n in alln) {
    x <- rnorm(n, mean = theta, sd= sigma_0)
    ## fill all the necessary code in
  }
}

## ---- q2
set.seed(1986)
theta <- 2
sigma_0 <- 3

alln <- c(1, 2, 5, 10, 100, 10000)
for(case in c(1, 2)){
  if (case == 1) {
    prior_mu <- 2
    prior_tau <- 2
  } else if (case == 2) {
    prior_mu <- 5
    prior_tau <- 1
  }
  
  for (n in alln) {
    x <- rnorm(n, mean = theta, sd= sigma_0)
    x_bar <- mean(x)
    
    a <- (n * x_bar)/sigma_0^2 + prior_mu/prior_tau^2
    b <- n/sigma_0^2 + 1/prior_tau^2
      
    post_mu <- a/b
    print(post_mu)
    post_sigma <- 1/(n/sigma_0^2 + 1/prior_tau^2)
      
    xx <- seq(-5, 5, by = 0.001)
    xx_prior <- xx * prior_tau + prior_mu
    xx_post <-  xx * post_sigma + post_mu
    
    Y <- cbind(dnorm(xx_prior, mean = prior_mu, sd= prior_tau), 
               dnorm(xx_post, mean = post_mu, sd = post_sigma))
    X <- cbind(xx_prior, xx_post)
    matplot(X, Y, type = 'l', lty = 1, main = paste("n = ", n))
    abline(v = x_bar, lty = 1)
  }
}


## ---- q3
theta <- seq(0, 1, .01)
dens <- theta^3 * (1-theta)^13 + 10 * theta^4 * (1-theta)^12 + 45 * theta^5 * (1-theta)^11
plot(theta, dens, ylim=c(0,1.1*max(dens)), type="l", xlab="theta", ylab="", xaxs="i",yaxs="i", yaxt="n", bty="n", cex=2)

## ---- q4
theta <- seq(0,1,.001)
dens <- dbeta(theta,1,.67)
plot (theta, dens, xlim=c(0,1), ylim=c(0,3),
type="l", xlab="theta", ylab="", xaxs="i",
yaxs="i", yaxt="n", bty="n", cex=2)
lines (c(1,1),c(0,3),col=0)
lines (c(1,1),c(0,3),lty=3)

## ---- q5
n <- 200
DT <- data.frame(c(0, 1, 2, 3, 4), c(109, 65, 22, 3, 1))
xbar <- sum(DT[, 1] * DT[, 2])/n

x <- seq(0, 2, by = 0.01)

for(case in c(1, 2, 3, 4)){
  if(case == 1){
    alpha <- beta <- 0.5 
  }else if(case == 2){
    alpha <- beta <- 1
  }else if(case == 3){
    alpha <- beta <- 10
  }else if(case == 4){
    alpha <- beta <- 100
  }
  
  dens <- dgamma(x, shape = alpha, rate = beta)

  alpha_posterior <- alpha + n * xbar
  beta_posterior <- beta + n
  dens_posterior <- dgamma(x, shape = alpha_posterior, rate = beta_posterior)
  
  matplot(x, cbind(dens, dens_posterior), lty = 1, type = 'l', ylab = "Density", xlab = "theta")
  abline(v = xbar)
  
}
