## ---- warmup
library(tidyr)
library(dplyr)
library(ggplot2)
# setwd("~/Teaching/Statistical_Thinking/data/")
student2012.sub <- readRDS("../../data/student_sub.rds") 
australia <- student2012.sub[student2012.sub$CNT=="AUS",]
australia <- australia %>% 
  mutate(math = (PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH)/5)


australia <- australia %>% 
  select(math, ST04Q01, ST06Q01, ST57Q01, ST15Q01, ST19Q01, ST26Q01, ST26Q02, ST26Q04, ST26Q06, ST27Q02, ST28Q01, SENWGT_STU)
australia$ST06Q01[australia$ST06Q01 > 9990] <- NA
australia$ST57Q01[australia$ST57Q02 > 9990] <- NA
australia$ST15Q01[australia$ST15Q01 > 6] <- NA
australia$ST19Q01[australia$ST19Q01 > 6] <- NA
australia$ST26Q01[australia$ST26Q01 > 6] <- NA
australia$ST26Q02[australia$ST26Q02 > 6] <- NA
australia$ST26Q04[australia$ST26Q04 > 6] <- NA
australia$ST26Q06[australia$ST26Q06 > 6] <- NA
australia$ST27Q02[australia$ST27Q02 > 6] <- NA
australia$ST28Q01[australia$ST28Q01 > 6] <- NA
australia <- australia %>% select(-ST57Q01)
aus_nomiss <- australia %>% filter(!is.na(ST04Q01)) %>%
  filter(!is.na(ST06Q01)) %>% filter(!is.na(ST15Q01)) %>%
  filter(!is.na(ST19Q01)) %>% filter(!is.na(ST26Q01)) %>%
  filter(!is.na(ST26Q02)) %>% filter(!is.na(ST26Q04)) %>%
  filter(!is.na(ST26Q06)) %>% filter(!is.na(ST27Q02)) %>%
  filter(!is.na(ST28Q01))


aus_nomiss$ST04Q01 <- factor(aus_nomiss$ST04Q01)
aus_nomiss$ST15Q01 <- factor(aus_nomiss$ST15Q01)
aus_nomiss$ST19Q01 <- factor(aus_nomiss$ST19Q01)
aus_nomiss$ST26Q01 <- factor(aus_nomiss$ST26Q01)
aus_nomiss$ST26Q02 <- factor(aus_nomiss$ST26Q02)
aus_nomiss$ST26Q04 <- factor(aus_nomiss$ST26Q04)
aus_nomiss$ST26Q06 <- factor(aus_nomiss$ST26Q06)
aus_nomiss <- aus_nomiss %>% mutate(math_std = (math-mean(math))/sd(math))
aus_nomiss$ST06Q01 <- aus_nomiss$ST06Q01 - 4

## ---- glm 
aus_glm <- glm(
  math_std ~ ST04Q01 + ST06Q01 + ST15Q01 + ST19Q01 + ST26Q01 +
             ST26Q04 + ST26Q06 + ST27Q02 + ST28Q01, 
  data = aus_nomiss, 
  weights = SENWGT_STU
  )
coef <- summary(aus_glm)$coefficients
coef

## ---- classical-ci
n <- nrow(aus_nomiss) # no. of observations
beta_14 <- coef[14, 1] # coefficient
se_14 <- coef[14, 2] # standard error
df <- n - 14 # degree of freedom = n - (k + 1)
t_crit <- qt(0.975, df) # t critical value
c(beta_14 - t_crit * se_14, beta_14 + t_crit * se_14) # (lower, upper)

## ---- q2
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

## ---- q2-actual
library(boot)
calc_stat <- function(d, i) {
  x <- d[i,]
  mod <- glm(
    math_std ~ ST04Q01 + ST06Q01 + ST15Q01 + ST19Q01 + ST26Q01 +
               ST26Q04 + ST26Q06 + ST27Q02 + ST28Q01, 
    data = x, 
    weights = SENWGT_STU
    )
  stat <- as.numeric(coefficients(mod)[14])
  return(stat)
}
stat <- boot(aus_nomiss, statistic = calc_stat, R = 1000,
             weights = aus_nomiss$SENWGT_STU)
stat

## ---- q2-show
c(sort(stat$t)[25], sort(stat$t)[975])

## ---- q3
calc_pred <- function(d, i, newd) {
  x <- d[i,]
  mod <- FILL IN THE NECESSARY CODE
  pred <- FILL IN THE NECESSARY CODE
  return(pred)
}

## ---- q3-pred
calc_pred <- function(d, i, newd) {
  x <- d[i,]
  mod <- glm(
    math_std ~ ST04Q01 + ST06Q01 + ST15Q01 + ST19Q01 + ST26Q01 +
               ST26Q04 + ST26Q06 + ST27Q02 + ST28Q01, 
    data = x, 
    weights = SENWGT_STU
    )
  pred <- predict(mod, newd)
  return(pred)
}

## ---- q3-data
new_data <- data.frame(
  ST04Q01 = factor(1), ST06Q01 = 0, ST15Q01 = factor(1), ST19Q01 = factor(1), 
  ST26Q01 = factor(1), ST26Q04 = factor(1), ST26Q06 = factor(1), ST27Q02 = 3, 
  ST28Q01 = 3, math_std = 0, SENWGT_STU = 0.1041
  )
pred_ci <- boot(aus_nomiss, statistic = calc_pred, R = 1000,
                weights = aus_nomiss$SENWGT_STU, newd = new_data)
pred_ci
c(sort(pred_ci$t)[25], sort(pred_ci$t)[975])

## ---- q4-template
calc_res <- function(d, i) {
  x <- d[i,]
  mod <- FILL IN THE NECESSARY CODE
  res <- FILL IN THE NECESSARY CODE
  return(res)
}

## ---- q4-pi
calc_res <- function(d, i) {
  x <- d[i,]
  mod <- glm(
    math_std ~ ST04Q01 + ST06Q01 + ST15Q01 + ST19Q01 + ST26Q01 +
               ST26Q04 + ST26Q06 + ST27Q02 + ST28Q01, 
    data = x, 
    weights = SENWGT_STU
    )
  res <- residuals(mod)
  return(c(l = min(res), u = max(res)))
}
res <- boot(aus_nomiss, statistic = calc_res, R = 1000,
            weights = aus_nomiss$SENWGT_STU)
l <- sort(res$t[, 1])[25]
u <- sort(res$t[, 2])[925]
actual_pred <- predict(aus_glm, new_data)
c(actual_pred + l, actual_pred + u)

