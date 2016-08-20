---
title: "ETC2420/5242 <br> Statistical distribution in R"
author: Earo Wang <br> slides on <http://bit.ly/etc2420-lab04>
date: Lab 04
---



# Statistical distributions

<!-- <center><img src="figure/pnorm.png" alt="normal" style="width: 1400px; height: 490px"/></center> -->



## Compute the probabilities for $N(2, 0.5)$

<img src="figure/ex-plot-1.png" title="plot of chunk ex-plot" alt="plot of chunk ex-plot" style="display: block; margin: auto;" />

+ $\Pr(X < 1.3)$


```r
pnorm(1.3, mean = 2, sd = 0.5)
```

```
## [1] 0.08075666
```

+ $\Pr(X > 1.9) = 1 - \Pr(X \le 1.9)$


```r
pnorm(1.9, mean = 2, sd = 0.5, lower.tail = FALSE)
```

```
## [1] 0.5792597
```

```r
1 - pnorm(1.9, mean = 2, sd = 0.5)
```

```
## [1] 0.5792597
```

+ $\Pr(1.8 < X < 2.2) = \Pr(X < 2.2) - \Pr(X < 1.8)$


```r
pnorm(2.2, mean = 2, sd = 0.5) - pnorm(1.8, mean = 2, sd = 0.5)
```

```
## [1] 0.3108435
```

## Compute the quantile values for $N(-3, 4)$


```r
qnorm(c(0.53, 0.12, 0.84, 1.2), mean = -3, sd = 4)
```

```
## [1] -2.6989206 -7.6999472  0.9778315        NaN
```

## Compute the densities for $N(-1.2, 0.8)$


```r
dnorm(c(-1, -0.2, -2), mean = -1.2, sd = 0.8)
```

```
## [1] 0.4833351 0.2283114 0.3024634
```

## Plot the density curves for Weibull


```r
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
```

<img src="figure/ex-weibull-1.png" title="plot of chunk ex-weibull" alt="plot of chunk ex-weibull" style="display: block; margin: auto;" />

# QQ-plot

## Simulate sample of size $n = 500$ from $\Gamma(2, 4)$


```r
set.seed(123)
n <- 500
alpha <- 2
beta <- 4
df_gamma <- data.frame(xgamma = rgamma(n, shape = alpha, rate = beta))
```

## QQ-Plot computation

1. Sort and standardise the sample values from low to high
2. Theoretical quantiles, $n=$ sample size
$$  1 - 0.5^{(1/n)} ~~~ i=1 \\
   ~~~~~~~~~~~~~\frac{i - 0.3175}{n + 0.365} ~~~ i=2, ..., n-1 \\
   0.5^{(1/n)} ~~~~~~ ~~~  i=n $$


```r
df_gamma$xq <- qgamma(c(1 - 0.5^(1/n), # i = 1
                        (2:(n-1) - 0.3175) / (n + 0.365), # i = 2, ... , n - 1
                        0.5^(1/n)), # i = n
                      alpha, beta) # theoretical quantiles
```
3. Plot the theoretical vs sample quantiles 


```r
df_gamma %>% 
  ggplot(aes(x = sort(xgamma), y = xq)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + # X = Y line
  xlab("Sample quantiles") +
  ylab("Theoretical quantiles") +
  coord_equal()
```

<img src="figure/q1-c-1.png" title="plot of chunk q1-c" alt="plot of chunk q1-c" style="display: block; margin: auto;" />

# Likelihood function


```r
set.seed(123)
X2 <- data.frame(x = rgamma(n = 267, 1.2, 0.25))
```

## Histogram for the sample

<img src="figure/q2-hist-1.png" title="plot of chunk q2-hist" alt="plot of chunk q2-hist" style="display: block; margin: auto;" />

## Compute the likelihood function

$$
\begin{aligned}
L(\alpha, \beta) &=\Pr(X_1=x_1,X_2=x_2, ... ,X_n=x_n~\big\vert~\alpha, \beta) \\      
&=f(x_1\big\vert\alpha, \beta)f(x_2\big\vert\alpha, \beta)\cdots f(x_n\big\vert\alpha, \beta) \\
&=\prod_{i=1}^n f(x_i;\alpha, \beta)
\end{aligned}
$$


```r
nmle <- function(x, alpha, beta) {
  # L(alpha, beta) = product of joint density functions
}
```




```r
alpha <- seq(0.9, 1.5, 0.01)
beta <- seq(0.18, 0.32, 0.005)
g <- expand.grid(x = alpha, y = beta) # all possible combinations of alpha and beta
g$l <- 0
for (i in 1:nrow(g)) {
  g$l[i] <- nmle(X2$x, g$x[i], g$y[i])
}
```

## Plot the likelihood function for a range of values of $\alpha$ and $\beta$

<img src="figure/q2-plot2-1.png" title="plot of chunk q2-plot2" alt="plot of chunk q2-plot2" style="display: block; margin: auto;" />

## Interactive 3D surface plot using `plotly`

<iframe src="3dscatter.html" height = "100%" width = "70%" align = "middle">
</iframe>

## Find the MLE estimates for $\alpha$, $\beta$


```r
library(MASS)
fitdistr(X2$x, "gamma")
```

```
##      shape         rate   
##   1.23994762   0.28051981 
##  (0.09624333) (0.02667898)
```

p.s. if you have trouble in installing the `CASdatasets` package, the following
code may solve the issue.


```r
# install.packages("xts")
# install.packages("sp")
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type = "source")
library(CASdatasets)
data(danishuni)
```

# Misc

## Online R resources

* [A list of distribution functions in R](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html)
* [Introduction to R Graphics with ggplot2](http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html)
* [Cookbook for R](http://www.cookbook-r.com/Graphs/)

## Consultation hours

* Earo
    * **Time**: 13:30 to 15:00 on Thursday
    * **Venue**: Level 11/W1106, Menzies Building
* Nathaniel
    * **Time**: 14:00 to 15:00 on Tuesday
    * **Venue**: Level 11/W1106, Menzies Building

<meta name="copyright" content="LICENSE: CC BY-NC 3.0 US" />
