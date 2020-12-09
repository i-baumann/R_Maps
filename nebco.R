library(ncdf4)
library(raster)
library(tidyverse)

# Heavily using https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html

setwd("/mnt/C6546276546268DF/gebco_2020_netcdf")

gebco <- raster("GEBCO_2020.nc")

# Create extent (our map area)
pr.e <- extent(-12, 37, 27.5, 46.85)

# Create a crop of the bathymetric data
pr.gebco <- crop(gebco, pr.e)

# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
colbr <- function(x, b1=50, b2=50, r1=-2, r2=-2) {
  # Min/max values of the raster (x)
  mi <- cellStats(x, stat="min")-100
  ma <- cellStats(x, stat="max")+100
  # Create sequences, but only use unique numbers
  s1 <- unique(round(seq(mi, 0, 0-mi/b1),r1))
  s2 <- unique(round(seq(0, ma, ma/b2),r2))
  # Combine sequence for our break points, removing duplicate 0
  s3 <- c(s1, s2[-1])
  # Create a list with the outputs
  # [[1]] = length of the first sequence minus 1 (water)
  # [[2]] = length of the second sequence minus 1 (land)
  # [[3]] = The break points
  x <- list(length(s1)-1, length(s2)-1, s3)
}

# Use function with default options
pr.br <- colbr(pr.gebco, b1=100, b2=100, r1=-1, r2=-1)

# Get country shapefiles
pr <- getData("GADM", country="PRT", level=0)
esp <- getData("GADM", country="ESP", level=0)  
# Colour palette
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
# Plot GEBCO_2019 coast of Portugal
plot(pr.gebco, col=c(blue.col(pr.br[[1]]), terrain.colors(pr.br[[2]])), breaks=pr.br[[3]])
#plot(pr, add=TRUE)
#plot(esp, add=TRUE)