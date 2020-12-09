library(tidyverse)
library(usmap)
library(raster)
library(png)
# If on Linux, may need to run:
# `sudo apt install libmagick++-dev`
library(magick)
# If on Linux, may need to run:
# `sudo apt-get install libcgal-dev libglu1-mesa-dev libglu1-mesa-dev`
library(rgl)
library(rayshader)
library(rayrender)
# If on Linux, may need to run:
# `sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev`
library(tidycensus)



# ENTER CENSUS API KEY
#################################

# If you do not have a Censu API key get one here:
# http://api.census.gov/data/key_signup.html

# You should only need to do this once

# census_api_key("YOUR KEY HERE", install = TRUE)

# PICK WHAT YEAR YOU WANT TO PLOT
#################################

election_year <- 2016



# Download and read in vote data
temp <- tempfile()

vote_data_url <- "https://dataverse.harvard.edu/api/access/datafile/3641280?format=RData&gbrecs=true"

download.file(vote_data_url, temp, method = "wget", quiet = TRUE)

load(temp)

votes <- x

rm(x)

# Create vote proportions
votes <- votes %>%
  filter(party %in% c("democrat", "republican"),
         year == election_year) %>%
  group_by(year, FIPS) %>%
  filter(party == "democrat") %>%
  group_by(year, FIPS) %>%
  mutate(dem_vote_prop = candidatevotes / totalvotes) %>%
  ungroup()

# Get county population data
pop <- get_estimates(geography = "county",
              product = "population",
              year = election_year)

pop <- pop %>%
  rename(population = value) %>%
  filter(variable == "POP")
pop$GEOID <- as.integer(pop$GEOID)

# Create base map data
counties <- us_map("counties")
states <- us_map("states")

counties <- as_tibble(counties)
counties$fips <- as.integer(counties$fips)

# Join population to counties
counties <- counties %>%
  left_join(pop %>%
              dplyr::select("GEOID",
                     "population"),
            by = c("fips" = "GEOID"))

# Join vote data to map data
counties <- counties %>%
  left_join(votes %>%
              dplyr::select(FIPS, dem_vote_prop),
            by = c("fips" = "FIPS"))

# Create logged population
counties$log_pop <- log(counties$population)

# Create map

counties_3d <- ggplot(data = counties, mapping = aes(x = x, y = y, group = group)) + 
  geom_polygon(color = "black", fill = "black")

counties_pop <- counties_3d +
  geom_polygon(data = counties,
               aes(fill = population),
               color = "black") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_continuous(low = "black",
                        high = "white") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        legend.position = "None") +
  theme(plot.background = element_rect(fill = "black")) +
  labs(title = paste(election_year, 
                     "Presidential Election", 
                     sep = " "),
       caption = "Only Republican and Democratic votes considered")
counties_pop
ggsave(filename = "elevation-2d.png", plot = counties_pop, width = 10, height = 6)

breaks = c(0,
           .25,
           .5,
           .75,
           1)

county_base <- counties_pop + 
  geom_polygon(data = counties,
               aes(fill = dem_vote_prop),
               color = "black") +
  scale_fill_gradientn(
    colors = c("red",
              "#ff0040",
              "purple",
              "#0040ff",
              "blue"),
    limits = c(min(counties$dem_vote_prop, na.rm = TRUE),
               max(counties$dem_vote_prop, na.rm = TRUE)),
                      breaks = breaks,
                      space = "Lab", 
                      na.value = "grey50", 
                      guide = "colourbar",
    name = "Dem.vote share") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        legend.position = c(.95,.5),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.9,"line")) +
  labs(title = paste(election_year, 
                     "Presidential Election", 
                     sep = " "),
       caption = "Only Republican and Democratic votes considered")
county_base
ggsave(filename = "election-2d.png", plot = county_base, width = 10, height = 6)


localtif <- raster::raster("elevation-2d.png")
elmat <- matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
                nrow=ncol(localtif),ncol=nrow(localtif))
elmat <- elmat[11:(nrow(elmat)-10),11:(ncol(elmat)-10)]

ecolor <- readPNG("election-2d.png")
ecolor <- ecolor[11:(nrow(ecolor)-10),11:(ncol(ecolor)-10),1:4]
ecolor[,,4] <- .9

elmat %>%
  sphere_shade(progbar = FALSE, 
               texture = "bw", 
               colorintensity=0.5) %>%
  add_overlay(overlay = ecolor) %>%
  add_shadow(ray_shade(elmat,
                       #maxsearch = 300,
                       #progbar = FALSE, 
                       #sunaltitude=20, 
                       #zscale=50, 
                       #sunangle = 45
                       ), 
             max_darken=0.2) %>%
  #add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, fov=30, theta=45, phi=25, windowsize=c(10000,10000), zoom=0.5,
          water=FALSE, waterdepth = 10, waterlinecolor = "white", waterlinealpha = 0.5,
          wateralpha = 0.8,watercolor = "lightblue")