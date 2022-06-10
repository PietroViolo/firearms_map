# Library
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

# POI : https://wiki.openstreetmap.org/wiki/Map_features

# building the query
q <- getbb("Montreal") %>%
  opq() %>%
  add_osm_feature("highway", "bus_stop")

str(q) #query structure


bus <- osmdata_sf(q)

#our background map
mon_map <- get_map(getbb("Montreal"), maptype = "toner-background")

#final map
ggmap(mon_map)+
  geom_sf(data = bus$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")



# West/South/East/North


#Nevada
m <- c(-119.93, 35.63, -114.06, 42)

#building the query
q <- m %>% 
  opq (timeout = 30*100) %>%
  add_osm_feature("amenity", "casino")

#query
vegas_gaming <- osmdata_sf(q)

#final map
ggplot(vegas_gaming$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)

# New York City Starbucks

nyc <- getbb("New York City") %>%
  opq(timeout = 30 * 100)  %>%
  add_osm_feature("amenity", "cafe")%>%
  add_osm_feature("name", "Starbucks")

nyc_starbucks <- osmdata_sf(nyc)


nyc_map <- get_map(getbb("New York City"), maptype = "toner-background")


ggmap(nyc_map)+
  geom_sf(data = nyc_starbucks$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")
