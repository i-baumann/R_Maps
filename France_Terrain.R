library(dplyr)
library(ggplot2)
library(raster)
library(rasterVis)
library(scales)
library(rgeos)

fr <- getData("GADM", country = "FRA", level = 0)

fr.c <- gCentroid(fr) %>% coordinates()

tile1 <- getData("SRTM",lat=fr.c[2],lon=fr.c[1]) # center of France SRTM tile
tile2 <- getData("SRTM", lat = 48.3904, lon = -4.4861) # Brest coordinates (Brittany tile)
tile3 <- getData("SRTM", lat = 43.4929, lon = -1.4748) # Bayonne coordinates (southwest France)
tile4 <- getData("SRTM", lat = 43.7102, lon = 7.2620) # Nice coordinates (cote azul)
tile5 <- getData("SRTM", lat = 43.6047, lon = 1.4442) # Toulouse coordinates (south France)
tile6 <- getData("SRTM", lat = 48.5734, lon = 7.7521) # Strasbourg coordinates (east France)
tile7 <- getData("SRTM", lat = 50.9513, lon = 1.8587) # Calais coordinates (north France)

fr.srtm <- merge(tile1,tile2,tile3,tile4,tile5,tile6,tile7)

fr.srtm <- aggregate(fr.srtm, fact=1.5)

fr.f <- crop(fr.srtm,fr)

fr.p  <-  rasterToPoints(fr.f)

fr.df <-  data.frame(fr.p)

colnames(fr.df) = c("lon", "lat", "alt")

minalt <- min(fr.df$alt)
maxalt <- max(fr.df$alt)



p1 <- ggplot(fr.df, aes(lon,lat)) +
  geom_raster(aes(fill = alt)) +
  scale_fill_gradientn(colours = terrain.colors(20)) +
  theme(panel.background = element_rect(fill = "#85abff"))