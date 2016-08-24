## ---- library
library(tidyr)
library(dplyr)
library(knitr)
library(broom)
library(purrr)
library(plotly)
library(ggplot2)

## ---- glimpse
library(gapminder)
glimpse(gapminder)

## ---- vis-country
ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country)) +
  geom_line(alpha = 0.5)

## ---- year1950
gapminder2 <- gapminder %>% mutate(year1950 = year - 1950)

## ---- au
oz <- gapminder2 %>% filter(country == "Australia")
head(oz)
## ---- au-plot
ggplot(data = oz, aes(x = year, y = lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

## ---- au-lm
oz_lm <- lm(lifeExp ~ year1950, data = oz) # Y = a + b*X
oz_lm # equivalent to print(oz_lm): prints only formula and coeffcients
summary(oz_lm) # returns detailed summary
class(oz_lm) # 'lm' object

## ---- au-predict
new_df <- data.frame(year1950 = c(0, 50, 66)) # 1950, 2000, 2016
predict(oz_lm, newdata = new_df)

# Manually calculate the predicted life expectancy in 1950, 2000, 2016
au_coef <- coefficients(oz_lm)
au_coef[1] + au_coef[2] * c(0, 50, 66)

## ---- tidy-lm
oz_coef <- tidy(oz_lm) # the tidy form of the lm object
oz_coef
oz_fit <- glance(oz_lm) # the diagnostics from lm object
oz_fit
oz_diag <- augment(oz_lm) # additional information like residuals
oz_diag

## ---- list
# df1 = a list of data.frame
list_df <- list(df1 = data.frame(x = 1:5, y = 1:5 + rnorm(5)))
# adding a new element called ols that is an lm object
list_df$ols <- lm(y ~ x, data = list_df$df1)
list_df

## ---- nest-data
by_country <- gapminder2 %>% 
  select(country, year1950, lifeExp, continent) %>%
  group_by(country, continent) %>% 
  nest() # nesting year1950 & lifeExp into a list
head(by_country)
head(by_country$data[[6]]) # data frame for Australia

## ---- map-lm
by_country <- by_country %>% 
  mutate(model = map(data, ~ lm(lifeExp ~ year1950, data = .)))
head(by_country)
by_country$model[[6]] # class: lm

## ---- map-tidy
country_coefs <- by_country %>% 
  unnest(model %>% map(tidy)) # pull list back to normal data frame
head(country_coefs)

## ---- report
country_coefs <- country_coefs %>% 
  select(country, continent, term, estimate) %>% 
  spread(term, estimate)
head(country_coefs) %>% kable() # knitr::kable helps to knit to markdown table

## ---- ggplot-colour
ggplot(country_coefs, aes(x = `(Intercept)`, y = year1950, colour = continent,
                          group = country)) +
  geom_point()

## ---- plotly
ggplotly()

## ---- diag
country_fit <- by_country %>% 
  unnest(model %>% map(glance))
head(country_fit)
