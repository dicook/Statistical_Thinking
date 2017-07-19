# Read stations data
library(readr)
stations <- read_table("../data/ghcnd-stations.txt",
                       col_names=c("ID", "lat", "lon", "elev", "state", "name",
                                   "v1", "v2", "v3"), skip=353, n_max=17081)

library(dplyr)
library(ggplot2)
oz <- map_data("world", xlim=range(stations$lon),
               ylim=range(stations$lat))
ggplot(oz, aes(x=long, y=lat)) + geom_path(aes(group=group)) +
  coord_quickmap() +
  geom_point(data=stations, aes(x=lon, y=lat),
             colour="red", alpha=0.5)

# Get pedestrian sensor locations
ped_loc <- read_csv("../data/Pedestrian_Sensor_Locations.csv")

library(ggmap)
melb <- get_map(location=c(mean(range(ped_loc$Longitude)),
                           mean(range(ped_loc$Latitude))), zoom=14)
ggmap(melb) + geom_point(data=ped_loc,
                         aes(x=Longitude, y=Latitude),
                         colour="#c51b7d", alpha=0.5, size=3)

# Choose a weather station close to pedestrian sensors
melb_stns <- stations %>% filter(lon > min(ped_loc$Longitude),
                                 lon < max(ped_loc$Longitude),
                                 lat > min(ped_loc$Latitude),
                                 lat < max(ped_loc$Latitude))

ggmap(melb) + geom_point(data=ped_loc,
                         aes(x=Longitude, y=Latitude),
                         colour="#c51b7d", alpha=0.5, size=3) +
  geom_point(data=melb_stns, aes(x=lon, y=lat),
             colour="#542788", size=6, shape=18)

# Read temp data
t2013 <- read_csv(file.choose(), col_names=FALSE)
t2013 <- t2013 %>% filter(X1 == melb_stns$ID)
t2014 <- read_csv(file.choose(), col_names=FALSE)
t2014 <- t2014 %>% filter(X1 == melb_stns$ID)
t2015 <- read_csv(file.choose(), col_names=FALSE)
t2015 <- t2015 %>% filter(X1 == melb_stns$ID)

melb_ghcn <- rbind(t2013, t2014, t2015)
write_csv(melb_ghcn, "../data/melb_ghcn.csv")

# Get pedestrian data - takes too long to have students do it
library(jsonlite)
limit <- 1453000 # all the up-to-date records need to be retrieved
limit <- 3000 # test it
web_add <- "https://data.melbourne.vic.gov.au/resource/mxb8-wn4w.json?"
ped_url <- paste0(web_add, "$limit=", limit)
pedestrian <- fromJSON(ped_url) # without api token
pedestrian <- tbl_df(pedestrian)
colnames(pedestrian) <- c("date_time", "day", "id", "mdate", "month", "count", "sensor_id", "sensor_name", "time", "year")
pedestrian <- pedestrian %>%
  mutate(date = as.Date(paste(pedestrian$mdate,
                              pedestrian$month,
                              pedestrian$year, sep="-"),
                        "%d-%b-%Y", tz = "AEST"),
         count = as.integer(count), sensor_id = factor(sensor_id))
summary(pedestrian$date)
ped_sub <- pedestrian %>% filter(year>2012, year<2016)
ggplot(ped_sub, aes(x=date, y=count)) + geom_point() +
facet_wrap(~sensor_name, ncol=4)
keep <- c("Bourke Street Mall (South)", "Melbourne Central", "Southern Cross Station", "Flagstaff Station", "Town Hall (West)", "Victoria Point", "Flinders Street Station Underpass", "Princes Bridge", "Webb Bridge", "Sandridge Bridge", "Waterfront City")
ped_sub <- ped_sub %>% filter(sensor_name %in% keep)
write_csv(ped_sub, "../data/pedestrian_counts_sub.csv")

# Other ideas

auscathist from CASdatasets Catastrophic events in Australia
examine the events by location and time
which are more expensive

pedestrian sensor data

zika virus incidence

melbourne temperature records

library(lubridate)
library(ggplot2)
glimpse(auscathist)
auscathist$FirstDay <- as.Date(auscathist$FirstDay)

ggplot(auscathist, aes(x=FirstDay, y=NormCost2014)) + geom_point()
ggplot(auscathist, aes(x=FirstDay, y=NormCost2014)) +
  geom_point() + facet_wrap(~Type, ncol=3)

