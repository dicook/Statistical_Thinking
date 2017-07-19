## ----setup, include = FALSE----------------------------------------------
library(knitr)
opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  echo=FALSE,
  fig.height = 2,
  fig.width = 5,
  collapse = TRUE,
  comment = "#>"
)
options(digits=2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(xkcd)

## ------------------------------------------------------------------------
olympics2008 <- read_csv("../data/olympics2008.csv")
olympics2012 <- read_csv("../data/olympics2012.csv")

oly <- merge(olympics2012[,c("Country","Total")],
             olympics2008[,c("Country","Total")], by="Country",
             all.x=TRUE)
colnames(oly)[2] <- "M2012"
colnames(oly)[3] <- "M2008"
oly <- oly %>% filter(!is.na(M2008))

oly_lm <- glm(M2012~M2008, data=oly)
library(broom)
kable(tidy(oly_lm))
coefs <- tidy(oly_lm)

## ----fig.align='center', fig.width==6, fig.height=4----------------------
ggplot(oly, aes(x=M2008, y=M2012)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + theme_xkcd() + xkcdaxis(c(0,110),c(0,110))

## ------------------------------------------------------------------------
oly_noUSA <- oly %>% filter(Country != "UnitedStates")
oly_noUSA_lm <- glm(M2012~M2008, data=oly_noUSA)
coefs_noUSA <- tidy(oly_noUSA_lm)
comp_estimates <- data.frame(all=coefs$estimate,
                             noUSA=coefs_noUSA$estimate)
comp_estimates$estimate <- c("intercept", "slope")
kable(comp_estimates, format = "markdown", row.names=TRUE)

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
comp_estimates_m <- comp_estimates %>% 
  gather(model, stat, -estimate) %>%
  spread(estimate, stat)
ggplot(oly, aes(x=M2008, y=M2012)) + geom_point() +
  geom_abline(data=comp_estimates_m, 
              aes(intercept=intercept, slope=slope, colour=model))  + theme_xkcd() + xkcdaxis(c(0,110),c(0,110))

## ------------------------------------------------------------------------
comp_diag <- data.frame(
  null.dev=c(glance(oly_lm)$null.deviance, glance(oly_noUSA_lm)$null.deviance),
  deviance=c(glance(oly_lm)$deviance, glance(oly_noUSA_lm)$deviance), 
  fitted=c(predict(oly_lm, oly[oly$Country=="UnitedStates",]), 
           predict(oly_noUSA_lm, oly[oly$Country=="UnitedStates",])), 
  resid=c(oly$M2012[oly$Country=="UnitedStates"]-
            predict(oly_lm, oly[oly$Country=="UnitedStates",]),
          oly$M2012[oly$Country=="UnitedStates"]-
            predict(oly_noUSA_lm, oly[oly$Country=="UnitedStates",])))
rownames(comp_diag) <- c("All", "No USA")
kable(comp_diag, format="markdown", row.names=TRUE)

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
set.seed(2468)
df <- data.frame(x=c(runif(99), 10))
df$y <- df$x+c(rnorm(99), 6)
df_all <- glm(y~x, data=df)
df_no <- glm(y~x, data=df[-100,])
df_est <- data.frame(all=tidy(df_all)$estimate,
                             no=tidy(df_no)$estimate)
df_est$estimate <- c("intercept", "slope")
df_est_m <- df_est %>% 
  gather(model, stat, -estimate) %>%
  spread(estimate, stat)
ggplot(df, aes(x=x, y=y)) + geom_point() +
  geom_abline(data=df_est_m, 
              aes(intercept=intercept, slope=slope, colour=model))  + theme_xkcd() + xkcdaxis(c(0,10),c(0,16))

## ------------------------------------------------------------------------
oly_diag <- augment(oly_lm)
oly_diag$Country <- oly$Country
oly_diag %>% arrange(desc(.hat)) %>% select(Country, .hat) %>% head(15)

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
ggplot(oly_diag, aes(x=.hat)) + geom_histogram() +
  geom_vline(xintercept=0.027, colour="red") + 
  theme_xkcd() + xkcdaxis(c(0,0.33),c(0,40))

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
oly_tf <- oly
oly_tf$M2008 <- log10(oly_tf$M2008)
oly_tf$M2012 <- log10(oly_tf$M2012)
ggplot(oly_tf, aes(x=M2008, y=M2012)) + geom_point() + 
  xlab("Log counts 2008") + ylab("Log counts 2012") + 
  theme_xkcd() + xkcdaxis(c(0,2.2),c(0,2.2))

## ------------------------------------------------------------------------
oly_tf_lm <- glm(M2012~M2008, data=oly_tf)
oly_tf_diag <- augment(oly_tf_lm)
oly_tf_diag$Country <- oly$Country
oly_tf_diag %>% arrange(desc(.hat)) %>% select(Country, .hat) %>% head(15)

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
ggplot(oly_tf_diag, aes(x=.hat)) + geom_histogram() +
  geom_vline(xintercept=0.027, colour="red") + 
  theme_xkcd() + xkcdaxis(c(0,.10),c(0,25))

## ----fig.align='center', fig.width=6, fig.height=4-----------------------
df_lm <- glm(y~x, data=df)
df_diag <- augment(df_lm)
ggplot(df_diag, aes(x=.hat)) + geom_histogram() +
  geom_vline(xintercept=0.02, colour="red") + 
  theme_xkcd() + xkcdaxis(c(0,1),c(0,100))

## ------------------------------------------------------------------------
tbl1 <- oly_diag %>% arrange(desc(.cooksd)) %>% select(Country, .cooksd) 
tbl2 <- oly_tf_diag %>% arrange(desc(.cooksd)) %>% select(Country, .cooksd)
tbl <- cbind(tbl1, tbl2)
kable(tbl)

## ----fig.align='center', fig.width=10, fig.height=4----------------------
p1 <- ggplot(oly_diag, aes(x=.cooksd)) + geom_histogram() + 
  theme_xkcd() + xkcdaxis(c(0,0.9),c(0,60))
p2 <- ggplot(oly_tf_diag, aes(x=.cooksd)) + geom_histogram() +
  theme_xkcd()+ xkcdaxis(c(0,0.2),c(0,25))
grid.arrange(p1, p2, ncol=2)

## ----fig.align='center', fig.width=10, fig.height=4----------------------
p1 <- ggplot(df_diag, aes(x=.cooksd)) + geom_histogram() + 
  ggtitle("All cases") + theme_xkcd() + xkcdaxis(c(0,30),c(0,100))
df_lm <- glm(y~x, data=df[-100,])
df_diag <- augment(df_lm)
p2 <- ggplot(df_diag, aes(x=.cooksd)) + geom_histogram() + ggtitle("Without the outlier") + theme_xkcd() +
  xkcdaxis(c(0,07),c(0,35))
grid.arrange(p1, p2, ncol=2)

## ------------------------------------------------------------------------
olympics2008 <- read_csv("../data/olympics2008.csv")
olympics2012 <- read_csv("../data/olympics2012.csv")

gdp2008 <- read_delim("../data/gdp2008.csv", delim="\t",
                      col_names=FALSE)[,3:4]
colnames(gdp2008) <- c("Country", "GDP")
oly_gdp2008 <- merge(olympics2008, gdp2008, by="Country", all.x=TRUE)

gdp2012 <- read_delim("../data/gdp2012.csv", delim="\t",
                      col_names=FALSE)[,1:2]
colnames(gdp2012) <- c("Country", "GDP")
oly_gdp2012 <- merge(olympics2012, gdp2012, by="Country", all.x=TRUE)

oly_gdp2012 <- merge(oly_gdp2012, olympics2008[,c("Country","Total")],
                     by="Country", all.x=TRUE)
colnames(oly_gdp2012)[7] <- "M2012"
colnames(oly_gdp2012)[13] <- "M2008"
oly_gdp2012[oly_gdp2012$Country=="HongKongChina", "Population"] <- 7155000
oly_gdp2012[is.na(oly_gdp2012$M2008), "M2008"] <- 0
oly_gdp2012 <- oly_gdp2012 %>%
  mutate(Pop_std=scale(Population), GDP_std=scale(GDP))
oly12_lm <- glm(M2012~M2008+Pop_std+GDP_std, data=oly_gdp2012)
oly12_lm_est <- tidy(oly12_lm)$estimate
kable(tidy(oly12_lm))

## ----fig.align='center', fig.width=10, fig.height=5----------------------
library(GGally)
ggscatmat(oly_gdp2012, columns=13:15) 

## ----fig.align='center', fig.width=4, fig.height=4-----------------------
p1 <- ggplot(oly_gdp2012, aes(x=M2008, y=Pop_std, label=Country)) + geom_point()
library(plotly)
ggplotly(p1)

## ------------------------------------------------------------------------
library(car)
vif(oly12_lm)

## ----fig.align='center', fig.width=4, fig.height=4-----------------------
olympics2004 <- read_csv("../data/olympics2004.csv")
olympics2004$Code <- substr(olympics2004$Country, 1, 3)
oly_gdp2012 <- merge(oly_gdp2012, olympics2004[,c("Code","Total")],
                     by="Code", all.x=TRUE)
colnames(oly_gdp2012)[16] <- "M2004"
write.csv(oly_gdp2012, file="../data/olympics_gdp_all.csv", 
          quote=FALSE, row.names=FALSE)
ggplot(oly_gdp2012, aes(x=M2004, y=M2008)) + geom_point() + 
  theme_xkcd() + xkcdaxis(c(0,110),c(0,110))

## ------------------------------------------------------------------------
oly12_lm <- glm(M2012~M2008+M2004+Pop_std+GDP_std, data=oly_gdp2012)
oly12_lm_est <- tidy(oly12_lm)$estimate
kable(tidy(oly12_lm))

## ------------------------------------------------------------------------
vif(oly12_lm)

