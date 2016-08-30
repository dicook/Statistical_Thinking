## ---- library
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)

## ---- wd
getwd() # getting the current working directory

## ---- read
student2012.sub <- readRDS("../../data/student_sub.rds") 
australia <- student2012.sub %>% 
  filter(CNT == "AUS")
summary(australia)

## ---- summary
summary(australia[, 1:4])

## ---- mat
library(GGally)
ggscatmat(australia, columns = 35:39)

## ---- math
australia <- australia %>% 
  mutate(math = (PV1MATH + PV2MATH + PV3MATH + PV4MATH + PV5MATH) / 5)

## ---- school
aus_schools <- australia %>% 
  group_by(SCHOOLID) %>% 
  tally() %>% 
  arrange(desc(n)) 
dim(aus_schools)

ggplot(aus_schools, aes(x = n)) + geom_histogram()

## ---- na
australia <- australia %>% 
  select(math, ST04Q01, ST06Q01, ST57Q01, ST15Q01, ST19Q01, ST26Q01, ST26Q02, 
         ST26Q04, ST26Q06, ST27Q02, ST28Q01, SENWGT_STU)
australia$ST06Q01[australia$ST06Q01 > 9990] <- NA
australia$ST57Q01[australia$ST57Q01 > 9990] <- NA
australia$ST15Q01[australia$ST15Q01 > 6] <- NA
australia$ST19Q01[australia$ST19Q01 > 6] <- NA
australia$ST26Q01[australia$ST26Q01 > 6] <- NA
australia$ST26Q02[australia$ST26Q02 > 6] <- NA
australia$ST26Q04[australia$ST26Q04 > 6] <- NA
australia$ST26Q06[australia$ST26Q06 > 6] <- NA
australia$ST27Q02[australia$ST27Q02 > 6] <- NA
australia$ST28Q01[australia$ST28Q01 > 6] <- NA

## ---- tally
australia %>% group_by(ST04Q01) %>% tally()
australia %>% group_by(ST06Q01) %>% tally()
australia %>% group_by(ST57Q01) %>% tally()
australia %>% group_by(ST15Q01) %>% tally()
australia %>% group_by(ST19Q01) %>% tally()
australia %>% group_by(ST26Q01) %>% tally()
australia %>% group_by(ST26Q02) %>% tally()
australia %>% group_by(ST26Q04) %>% tally()
australia %>% group_by(ST26Q06) %>% tally()
australia %>% group_by(ST27Q02) %>% tally()
australia %>% group_by(ST28Q01) %>% tally()

## ---- tally1
count_by <- function(group_var) {
  require(lazyeval)
  australia %>% group_by_(group_var) %>% tally()
}
groups <- colnames(australia)[2:12]
groups %>% map(count_by)

## ---- var
australia <- australia %>% select(-ST57Q01) # drop ST57Q01
aus_nomiss <- australia %>% filter(!is.na(ST04Q01)) %>%
  filter(!is.na(ST06Q01)) %>% filter(!is.na(ST15Q01)) %>%
  filter(!is.na(ST19Q01)) %>% filter(!is.na(ST26Q01)) %>%
  filter(!is.na(ST26Q02)) %>% filter(!is.na(ST26Q04)) %>%
  filter(!is.na(ST26Q06)) %>% filter(!is.na(ST27Q02)) %>%
  filter(!is.na(ST28Q01))

## ---- var1
aus_nomiss <- australia %>% 
  filter(complete.cases(.))

## ---- dim
dim(australia); dim(aus_nomiss)

## ---- tran
aus_nomiss$ST04Q01 <- factor(aus_nomiss$ST04Q01)
aus_nomiss$ST15Q01 <- factor(aus_nomiss$ST15Q01)
aus_nomiss$ST15Q01 <- factor(aus_nomiss$ST15Q01)
aus_nomiss$ST19Q01 <- factor(aus_nomiss$ST19Q01)
aus_nomiss$ST26Q01 <- factor(aus_nomiss$ST26Q01)
aus_nomiss$ST26Q02 <- factor(aus_nomiss$ST26Q02)
aus_nomiss$ST26Q04 <- factor(aus_nomiss$ST26Q04)
aus_nomiss$ST26Q06 <- factor(aus_nomiss$ST26Q06)
aus_nomiss <- aus_nomiss %>% mutate(math_std = (math - mean(math)) / sd(math))
aus_nomiss$ST06Q01 <- aus_nomiss$ST06Q01 - 4

## ---- model
aus_lm <- lm(math_std ~ ST04Q01 + ST26Q04 + ST28Q01, data = aus_nomiss)
aus_lm_aug <- augment(aus_lm)
ggplot(aus_lm_aug, aes(x = .fitted, y = .resid)) +
  geom_point()

## ---- hist
ggplot(aus_nomiss, aes(x = SENWGT_STU)) + geom_histogram()


## ---- model1
aus_glm_test <- glm(math_std ~ ST04Q01 + ST26Q04 + ST28Q01, data = aus_nomiss, 
                    weights = SENWGT_STU)
summary(aus_glm_test)

## ---- inter
ggplot(aus_nomiss, 
       aes(x = ST28Q01, y = math_std, colour = interaction(ST04Q01, ST26Q04))) +
  geom_smooth(method = "lm", se = FALSE)
