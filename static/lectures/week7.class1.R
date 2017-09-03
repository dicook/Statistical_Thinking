## ----setup, include = FALSE----------------------------------------------
library(knitr)
opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  echo=FALSE,
  fig.height = 4,
  fig.width = 4,
  fig.align='center'
)
options(digits=3)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(broom)

## ------------------------------------------------------------------------
library(HLMdiag)
data(radon)
radon <- radon %>% rename(storey=basement)

## ------------------------------------------------------------------------
glimpse(radon)

## ------------------------------------------------------------------------
ggplot(radon, aes(x=uranium, y=log.radon)) + geom_point()

## ------------------------------------------------------------------------
radon_keep <- radon %>% group_by(county) %>% 
  tally() %>% filter(n > 4)
radon_sub <- radon %>% 
  filter(county %in% radon_keep$county & log.radon > -2)
radon_sub$storey <- 
  factor(radon_sub$storey, levels=c(0,1), 
         labels=c("basement", "first floor"))

## ----fig.width=8---------------------------------------------------------
ggplot(radon_sub, aes(x=uranium, y=log.radon)) + 
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(~storey)

## ------------------------------------------------------------------------
radon_lm <- glm(log.radon ~ storey + uranium, 
                data = radon_sub)
summary(radon_lm)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## radon_lm_fit <- radon_sub; radon_lm_fit$fit <- fitted(radon_lm)
## ggplot(radon_lm_fit, aes(x=uranium, y=log.radon, colour=storey)) + geom_point() +
##   geom_line(aes(y=fit))

## ------------------------------------------------------------------------
radon_lm <- glm(log.radon ~ storey*uranium, data = radon_sub)
summary(radon_lm)
radon_lm_fit <- radon_sub; radon_lm_fit$fit <- fitted(radon_lm)

## ----fig.width=6---------------------------------------------------------
ggplot(radon_lm_fit, aes(x=uranium, y=log.radon, colour=storey)) +
  geom_point(alpha=0.3) + 
  geom_line(aes(y=fit), size=1)

## ----echo=TRUE, results='hide'-------------------------------------------
library(lme4)
radon_lmer <- lmer(log.radon ~ storey + uranium + 
  (storey | county.name), data = radon_sub)
summary(radon_lmer)
radon_lmer_fit <- augment(radon_lmer) 

## ----eval=FALSE, echo=FALSE----------------------------------------------
## # p=2 (storey, uranium)
## # q=1 (storey)
## length(unique(radon_sub$county.name)) # g
## radon_sub %>% group_by(county.name) %>% tally()

## ----fig.width=7---------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=uranium, y=log.radon)) + 
  geom_point(alpha=0.2) + 
  geom_point(aes(y=.fitted, colour=county.name)) + 
  facet_wrap(~storey) + theme(legend.position="none")

## ----fig.width=6---------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=uranium, y=log.radon)) + 
  geom_point(alpha=0.2) + 
  geom_point(aes(y=.fitted, colour=storey)) 

## ----fig.width=6---------------------------------------------------------
ggplot(radon_lm_fit, aes(x=uranium, y=log.radon, colour=storey)) +
  geom_point(alpha=0.3) + 
  geom_line(aes(y=fit), size=1)

## ----fig.show='hide'-----------------------------------------------------
radon_lmer_fit$resid1 <-  HLMresid(radon_lmer, 
           level=1)
ggplot(radon_lmer_fit, aes(x=resid1)) + 
  geom_histogram(binwidth=0.5) 

## ----echo=FALSE----------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=resid1)) + 
  geom_histogram(binwidth=0.5) 

## ------------------------------------------------------------------------
ggplot_qqnorm(radon_lmer_fit$resid1, line="rlm") +
  theme(aspect.ratio=1)

## ------------------------------------------------------------------------
radon_lmer_fit %>% group_by(county.name) %>%
  summarise(m = mean(resid1), s = sd(resid1), n = length(resid1)) %>%
  head()

## ----echo=FALSE, fig.height=6--------------------------------------------
res.sum <- radon_lmer_fit %>% group_by(county.name) %>%
  summarise(m = mean(resid1), s = sd(resid1), n = length(resid1))
ord <- order(res.sum$m)
radon_lmer_fit$county.name <- factor(radon_lmer_fit$county.name, levels=res.sum$county.name[ord])
ggplot(radon_lmer_fit, aes(x=county.name, y=resid1)) + 
  geom_point(alpha=0.5) + coord_flip()

## ----results='hide'------------------------------------------------------
library("nortest")
ad.test(radon_lmer_fit$resid1)
cvm.test(radon_lmer_fit$resid1)
lillie.test(radon_lmer_fit$resid1)

## ----echo=FALSE----------------------------------------------------------
ad.test(radon_lmer_fit$resid1)

## ------------------------------------------------------------------------
rf <- HLMresid(radon_lmer, level="county.name") 
# same as ranef(radon_lmer)
rf$county.name <- rownames(rf)
rf <- rf %>% rename(resid.basement=`(Intercept)`, 
                    resid.ff=`storeyfirst floor`)
radon_lmer_fit <- merge(radon_lmer_fit, rf, 
                        by="county.name")

## ----fig.width=8---------------------------------------------------------
p1 <- ggplot(radon_lmer_fit, aes(x=resid.basement)) + 
  geom_histogram(binwidth=0.05) + ggtitle("basement")
p2 <- ggplot(radon_lmer_fit, aes(x=resid.ff)) + 
  geom_histogram(binwidth=0.2) + ggtitle("first floor")
grid.arrange(p1, p2, ncol=2)

## ----fig.width=8---------------------------------------------------------
p1 <- ggplot_qqnorm(radon_lmer_fit$resid.basement, line="rlm") +
  theme(aspect.ratio=1) + ggtitle("basement")
p2 <- ggplot_qqnorm(radon_lmer_fit$resid.ff, line="rlm") +
  theme(aspect.ratio=1) + ggtitle("first floor")
grid.arrange(p1, p2, ncol=2)

## ------------------------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=resid.basement, y=resid.ff)) + 
  geom_point() + theme(aspect.ratio=1) 

## ----echo=FALSE----------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=.fitted, y=log.radon)) + 
  geom_point()  

## ------------------------------------------------------------------------
glance(radon_lm)

## ------------------------------------------------------------------------
glance(radon_lmer)

## ------------------------------------------------------------------------
sum(radon_lmer_fit$resid1^2)

## ------------------------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=.fitted, y=.cooksd)) + geom_point()

