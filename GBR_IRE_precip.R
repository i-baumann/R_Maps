library(dplyr)
library(ggplot2)
library(raster)
library(rasterVis)
library(scales)
library(rgeos)

gbr <- getData("GADM", country = "GBR", level = 1)

gbr.c <- gCentroid(gbr) %>% coordinates()

met1 <- getData("worldclim", var = "prec", res = .5, 
                lon = gbr.c[1], lat = gbr.c[2]) # climate data (most of the islands)

met2 <- getData("worldclim", var = "prec", res = .5, 
                lon = 1.627036, lat = 52.504069) # climate data (east England)

met <- merge(met1,met2)

gbr.met <- crop(met,gbr)

gbr.p  <-  rasterToPoints(gbr.met)

gbr.df <-  data.frame(gbr.p)

gbr.df$mean_prec <- rowMeans(gbr.df[3:14], na.rm = TRUE)

names(gbr.df)[1] = "lon"
names(gbr.df)[2] = "lat"

p1 <- ggplot(gbr.df, aes(lon,lat)) +
  geom_raster(aes(fill = mean_prec)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,max(gbr.df$mean_prec))) +
  theme_dark() #+
  #theme(panel.background = element_rect(fill = ""))