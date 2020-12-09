library(dplyr)
library(ggplot2)
library(raster)
library(rasterVis)
library(scales)
library(rgeos)

usa <- getData("GADM", country = "USA", level = 0)

usa.c <- gCentroid(usa) %>% coordinates()

met1 <- getData("worldclim", var = "prec", res = .5, 
                lon = usa.c[1], lat = usa.c[2]) # climate data (most of the US)

met2 <- getData("worldclim", var = "prec", res = .5, 
                lon = -123.352452, lat = 38.679868) # climate data (west coast)

met3 <- getData("worldclim", var = "prec", res = .5, 
                lon = -79.546732, lat = 38.269487) # climate data (east coast)

met4 <- getData("worldclim", var = "prec", res = .5, 
                lon = -118, lat = 49) # climate data (PNW)

met4 <- getData("worldclim", var = "prec", res = .5, 
                lon = -68, lat = 47) # climate data (NE)

met5 <- getData("worldclim", var = "prec", res = .5, 
                lon = -89, lat = 27) # climate data (Florida)

met6 <- getData("worldclim", var = "prec", res = .5, 
                lon = -100, lat = 25) # climate data (southern Texas)

met <- merge(met1, met2, met3, met4, met5, met6)

rm(met1, met2, met3)

usa.met <- crop(met,usa)

usa.p  <-  rasterToPoints(usa.met)

usa.df <-  data.frame(usa.p)

rm(usa.p, met, usa.p)

usa.df$mean_prec <- rowMeans(usa.df[3:14], na.rm = TRUE)

names(usa.df)[1] = "lon"
names(usa.df)[2] = "lat"

p1 <- ggplot(usa.df, aes(lon,lat)) +
  geom_raster(aes(fill = mean_prec)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,max(usa.df$mean_prec))) +
  theme_dark() +
  xlim(-126, -66) +
  ylim(24, 50) #+
  #theme(panel.background = element_rect(fill = ""))