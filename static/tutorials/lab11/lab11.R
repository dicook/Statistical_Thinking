## ---- data
library(tidyverse)
ped_weather <- read_csv("melb_ped_weather.csv")
ped_weather <- ped_weather %>% 
  mutate(
    time = factor(time),
    day = factor(day),
    month = factor(month)
  )

## ---- glm
ped_weath_glm <- glm(
  count ~ day * time * month * sensor
  + high_tmp + low_tmp + high_prcp,
  data = ped_weather, family = poisson(link = "log")
)
save(ped_weath_glm, file = "ped_weather_glm.rda")

## ---- smooth-plot
load("ped_weather_glm.rda")
ped_weather <- ped_weath_glm$data %>%
  mutate(
    day = factor(day, levels=c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")), 
    month = factor(month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
ped_weather$new1 <- simulate(ped_weath_glm)$sim_1
ggplot(ped_weather, 
  aes(x=as.numeric(as.character(time)), y=new1, colour=sensor)) + 
  xlab("Time") + 
  ylab("Count") +
  geom_smooth() +
  facet_grid(month ~ day) + 
  theme_bw() + 
  theme(legend.position="bottom")

## ---- boxplot
ped_weather %>% select(sensor, day, count, new1) %>%
  gather(type, value, count, new1) %>%
  ggplot(aes(x=day, y=value, colour=type)) + 
  geom_boxplot() + facet_wrap(~sensor)

## ---- simulate
ped_new <- simulate(ped_weath_glm, 10)
ped_new <- bind_cols(ped_weather, ped_new)