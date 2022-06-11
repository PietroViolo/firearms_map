# Library
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(raster)
library(terra)
library(plotwidgets)
library(ggshadow)
library(ggspatial)
library(ggnewscale)
library(janitor)
library(rnaturalearth)


# remotes::install_github('ropensci/osmdata')

# Thank you to Dominic R. for his tutorial

# POI : https://wiki.openstreetmap.org/wiki/Map_features
  
#Firearms united states
m <- c(-125.064, 23.0486, -63.2310, 49.7254)

#building the query
data1 <- m %>% 
  opq (timeout = 300*100) %>%
  add_osm_feature("shop", "weapons")

data2 <- m %>% 
  opq (timeout = 300*100) %>%
  add_osm_feature("shop", "hunting")

data3 <- m %>% 
  opq (timeout = 300*100) %>%
  add_osm_feature("shop", "military_surplus")

#query
firearm_data1 <- osmdata_sf(data1)
firearm_data2 <- osmdata_sf(data2)
firearm_data3 <- osmdata_sf(data3)

x<-firearm_data1$osm_points

# ATF data

FFL <- readxl::read_excel("0122-ffl-list.xlsx")
FFL <- FFL[-1,]





# Map making

map <- rast("snapshot-2022-05-14T00_00_00Z.tiff")

map <- crop(map, extent(-125.064, 23.0486, -63.2310, 49.7254))

# Unsaturated version of the map

saturation <- function(rgb, s = .5){
  
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  
  rgb_new <- as.vector(t(hsl2rgb(hsl)))
  
  return(rgb_new)
  
}

# apply the function to unsaturate with 5%

#map_desat <- app(map, saturation, s = .05)

plotRGB(map_desat)


bm_desat <- terra::project(map_desat, "epsg:3035")

limits <- ne_countries(scale = 50, returnclass = "sf")

bx <- tibble(x = c(-125.064, 23.0486), y = c(-63.2310, 49.7254)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(3035) %>% 
  st_bbox()

ggplot()  +
  layer_spatial(data = stack(map_desat))  +
  geom_glowpoint(data = firearm$osm_points,
                 aes(geometry = geometry),
                 alpha = 0.8,
                 color = "#de1d1d",
                 shadowcolour = "#de1d1d",
                 stat = "sf_coordinates",
                 shadowalpha = 0.1)+
  geom_glowpoint(data = firearm$osm_points,
                 aes(geometry = geometry),
                 alpha = 0.3,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 shadowalpha = 0.05) +
  theme_void() +
  theme(plot.title = element_text(size = 50, vjust = -5, colour = "white", hjust = .95))

ggsave("firearms_map_empty.png", width = 15, height = 15, units = "in", dpi = 300)
