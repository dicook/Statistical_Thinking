library(readr)
AH <- read_csv("Ames-housing-2015.csv")

library(dplyr)
glimpse(AH)

summary(AH$YrSold_YYYY)

AH_sold <- AH %>%
  filter(!is.na(YrSold_YYYY)) %>%
  filter(ClassPr_S == "RESIDENTIAL") %>%
  select(SalePrice, Neighborhood, LotArea, YrBuilt, HouseStyle, Foundation, RoofMatl, Ext1, Heating, `Central Air`, TtlBsmtSF, TotRmsAbvGrd, GarageType, Cars, GarageArea, NmbrBRs, YrSold_YYYY, MoSold_MM)

AH_sold %>% group_by(Neighborhood) %>% tally(sort=TRUE) %>% tail()
AH_sold %>% group_by(YrBuilt) %>% tally(sort=TRUE) %>% tail()
AH_sold %>% group_by(HouseStyle) %>% tally(sort=TRUE) %>% tail()
AH_sold %>% group_by(Foundation) %>% tally(sort=TRUE) %>% tail()

ggplot(AH_sold, aes(x=SalePrice)) + geom_histogram()

ggplot(AH_sold, aes(x=YrSold_YYYY, y=SalePrice)) + geom_point()

library(lubridate)
AH_sold <- AH_sold %>%
  mutate(SaleDate = dmy(paste("01-",AH_sold$MoSold_MM, "-",
                                  AH_sold$YrSold_YYYY, sep="")))
