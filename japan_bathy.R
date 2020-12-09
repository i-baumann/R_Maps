library(ncdf4)
library(raster)
library(tidyverse)

# Heavily using https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html

setwd("/mnt/C6546276546268DF/gebco_2020_netcdf")

gebco <- raster("GEBCO_2020.nc")

# Create extent (our map area)
jap.e <- extent(126.207266, 153.589775, 25.058187, 48.327208)

# Create a crop of the bathymetric data
jap.gebco <- crop(gebco, jap.e)
jap.p  <-  rasterToPoints(jap.gebco)

# Make plot

jap.df <-  data.frame(jap.p)

names(jap.df)[1] = "lon"
names(jap.df)[2] = "lat"
names(jap.df)[3] = "Altitude/Depth (relative to sea level)"

rm(jap.e, jap.gebco, jap.p)

p1 <- ggplot(jap.df, aes(lon,lat)) +
  geom_raster(aes(fill = `Altitude/Depth (relative to sea level)`)) +
  scale_colour_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +
  borders(database = "world", regions = c("Japan", "South Korea", "North Korea", "Russia", "China")) +
  xlim(126.207266, 153.589775) +
  ylim(25.058187, 48.327208)