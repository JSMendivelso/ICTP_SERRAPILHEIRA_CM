# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Spatial data
# First Version 2022-07-26
# --------------------------------------------------#


# Loading needed packages
library(sf)
library(tmap)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)


data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()

#head(World)
#names(World)
#class(World)
#dplyr::glimpse(World)

plot(World)
plot(World[1]) #Same output as plot(World[,1])
plot(World[1,]) #include max.plot = 15 argument to plot all the figures in the category
plot(World["pop_est"])

head(World[, 1:4])
head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
st_bbox(World)
unique(World$continent)

#Plotting only countries in South America
World %>% #The simbols allow us to take World as the first argument of the function
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

#Creating new variables to use them in the map
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX","ARG"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")
#if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) If the argument is in the condition, do "red", if not, do "grey"

bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)
dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

bra2 <- read_sf("data/shapefiles/bra.shp")

dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

is(tmax_data) #the data are a raster stack, several rasters piled
res(tmax_data) #resolution of the data

