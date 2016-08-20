library(tidyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(gapminder)
glimpse(gapminder)

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country)) +
  geom_line(alpha = 0.5)

gapminder2 <- gapminder %>% mutate(year1950 = year - 1950)

oz <- gapminder2 %>% filter(country == "Australia")
head(oz)
ggplot(data = oz, aes(x = year, y = lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
oz_lm <- lm(lifeExp ~ year1950, data = oz) # Y = a + b*X
oz_lm # prints only formula and coeffcients
summary(oz_lm) # returns detailed summary
class(oz_lm) # 'lm' object

library(broom)
oz_coef <- tidy(oz_lm) # the tidy form of the lm object
oz_coef
oz_fit <- glance(oz_lm) # the diagnostics from lm object
oz_fit
oz_diag <- augment(oz_lm) # additional information like residuals
oz_diag

library(purrr)
by_country <- gapminder2 %>% 
  select(country, year1950, lifeExp, continent) %>%
  group_by(country, continent) %>% 
  nest() # nesting year1950 & lifeExp into a list
head(by_country)
head(by_country$data[[1]])
by_country <- by_country %>% 
  mutate(model = map(data, ~ lm(lifeExp ~ year1950, data = .)))
by_country$model[[1]]
country_coefs <- by_country %>% 
  unnest(model %>% map(tidy)) # pull list back to normal data frame
country_coefs <- country_coefs %>% 
  select(country, continent, term, estimate) %>% 
  spread(term, estimate)
kable(head(country_coefs))
colnames(country_coefs)

ggplot(country_coefs, aes(x = `(Intercept)`, y = year1950, colour = continent)) +
  geom_point()

ggplot(country_coefs, aes(x = `(Intercept)`, y = year1950, colour = continent, 
                          label = country)) +
  geom_point()
library(plotly)
ggplotly()

country_fit <- by_country %>% 
  unnest(model %>% map(glance))
