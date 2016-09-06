---
title: "ETC2420/5242 <br> Bootstrapping in R"
author: Earo Wang <br> <earo.wang@gmail.com>
date: Lab 07
---



# Happy bootstrapping

<!-- break -->




```r
aus_glm <- glm(
  math_std ~ ST04Q01 + ST06Q01 + ST15Q01 + ST19Q01 + ST26Q01 +
             ST26Q04 + ST26Q06 + ST27Q02 + ST28Q01, 
  data = aus_nomiss, 
  weights = SENWGT_STU
  )
coef <- summary(aus_glm)$coefficients
coef
```

```
##                Estimate  Std. Error    t value      Pr(>|t|)
## (Intercept)  0.16820487 0.056138513   2.996247  2.738877e-03
## ST04Q012     0.21913103 0.016491266  13.287702  5.255760e-40
## ST06Q01     -0.10700244 0.012146342  -8.809437  1.431179e-18
## ST15Q012     0.08977190 0.019997268   4.489208  7.216001e-06
## ST15Q013    -0.21068542 0.044312283  -4.754560  2.012097e-06
## ST15Q014    -0.01886587 0.021576831  -0.874358  3.819410e-01
## ST19Q012    -0.07680318 0.034592158  -2.220248  2.642068e-02
## ST19Q013    -0.08173197 0.050768898  -1.609883  1.074500e-01
## ST19Q014    -0.16930988 0.034011827  -4.977971  6.515390e-07
## ST26Q012    -0.28638383 0.031398635  -9.120901  8.639815e-20
## ST26Q042    -0.37012730 0.066721745  -5.547326  2.962647e-08
## ST26Q062    -0.40839738 0.058624923  -6.966276  3.425858e-12
## ST27Q02     -0.21757546 0.012419415 -17.518978  7.270215e-68
## ST28Q01      0.21293890 0.006056394  35.159355 9.686558e-258
```

## Classical confidence intervals

$$
\hat{\beta}_{14} \pm t_{\alpha/2, n - (k + 1)} \times \mathrm{S.E.}(\hat{\beta}_{14})
$$


```r
n <- nrow(aus_nomiss) # no. of observations
beta_14 <- coef[14, 1] # coefficient
se_14 <- coef[14, 2] # standard error
df <- n - 14 # degree of freedom = n - (k + 1)
t_crit <- qt(0.975, df) # t critical value
c(beta_14 - t_crit * se_14, beta_14 + t_crit * se_14) # (lower, upper)
```

```
## [1] 0.2010674 0.2248104
```

## Hypothesis test

$$
\begin{align}
H_{0}:& \beta_{14} = 0 \\
H_{1}: & \beta_{14} \neq 0
\end{align}
$$

Compare $t_{crit}$ and $t_{test}$ and decide to reject $H_0$ or not.

## Confidence intervals via bootstrap

1. Make a N boostrap samples (sample data rows, with replacement)
2. Fit the model for each
3. Compute lower and upper C% bounds, by sorting values and pulling the relevant ones, e.g. if N = 1000, C = 95, we would take the 25$^{th}$ and 975$^{th}$ values as the lower and upper CI bounds


```r
library(boot)
calc_stat <- function(d, i) {
  x <- d[i,]
  mod <- FILL IN THE NECESSARY CODE
  stat <- FILL IN THE NECESSARY CODE
  return(stat)
}
stat <- boot(aus_nomiss, statistic = calc_stat, R = 1000,
             weights = aus_nomiss$SENWGT_STU)
stat
```


```
## 
## WEIGHTED BOOTSTRAP
## 
## 
## Call:
## boot(data = aus_nomiss, statistic = calc_stat, R = 1000, weights = aus_nomiss$SENWGT_STU)
## 
## 
## Bootstrap Statistics :
##      original       bias    std. error  mean(t*)
## t1* 0.2129389 -0.005265462 0.006844996 0.2076734
```


```r
c(sort(stat$t)[25], sort(stat$t)[975])
```

```
## [1] 0.1934315 0.2204962
```

## Confidence intervals for predicted value


```r
calc_pred <- function(d, i, newd) {
  x <- d[i,]
  mod <- FILL IN THE NECESSARY CODE
  pred <- FILL IN THE NECESSARY CODE
  return(pred)
}
```



Now make a 95% bootstrap confidence interval for predicted value for a new student who is FEMALE, started school at 4, mother and father both work full-time, has a desk, computer and internet, two TVs and 26-100 books in the home. The weight for a student like this is 0.1041. 


```r
new_data <- data.frame(
  ST04Q01 = factor(1), ST06Q01 = 0, ST05Q01 = 0, 
  ST15Q01 = factor(1), ST19Q01 = factor(1), ST26Q01 = factor(1), 
  ST26Q04 = factor(1), ST26Q06 = factor(1), ST27Q02 = 3, 
  ST28Q01 = 3, math_std = 0, SENWGT_STU = 0.1041
  )
pred_ci <- boot(aus_nomiss, statistic = calc_pred, R = 1000,
                weights = aus_nomiss$SENWGT_STU, newd = new_data)
pred_ci
```

```
## 
## WEIGHTED BOOTSTRAP
## 
## 
## Call:
## boot(data = aus_nomiss, statistic = calc_pred, R = 1000, weights = aus_nomiss$SENWGT_STU, 
##     newd = new_data)
## 
## 
## Bootstrap Statistics :
##      original     bias    std. error  mean(t*)
## t1* 0.1542952 0.03155665  0.02504244 0.1858518
```

```r
c(sort(pred_ci$t)[25], sort(pred_ci$t)[975])
```

```
## [1] 0.1382045 0.2340002
```
Be sure to convert the values back into the actual math score range.

## Prediction intervals

1. Compute the residuals from the fitted model 
2. Bootstrap the residuals
3. Find the desired quantiles of the residuals
4. Compute prediction intervals by adding residual quantiles to fitted value


```r
calc_res <- function(d, i) {
  x <- d[i,]
  mod <- FILL IN THE NECESSARY CODE
  res <- FILL IN THE NECESSARY CODE
  return(res)
}
```

<meta name="copyright" content="LICENSE: CC BY-NC 3.0 US" />
