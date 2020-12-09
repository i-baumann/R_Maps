library(tidyverse)
library(usmap)
library(jsonlite)

# PICK WHAT YEAR YOU WANT TO PLOT
#################################

# Download and read in vote data
raw_data <- fromJSON("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/results.json")

counties_data <- NULL

for (x in 1:51) {
  counties_temp <- as_tibble(raw_data[["data"]][["races"]][["counties"]][[x]])
  counties_data <- bind_rows(counties_data, counties_temp)
}

votes <- flatten(counties_data) %>%
  select(fips, results.trumpd, results.bidenj)

votes$dem_vote_prop <- votes$results.bidenj / (votes$results.trumpd +
                                                     votes$results.bidenj)

votes <- as_tibble(votes)
votes$fips <- as.integer(votes$fips)

# Create base map data
counties <- us_map("counties")
states <- us_map("states")
counties$fips <- as.integer(counties$fips)

# Join vote data to map data
counties <- counties %>%
  left_join(votes %>%
              select(fips, dem_vote_prop),
            by = "fips")

breaks = c(0, .25, .5, .75, 1)

county_base <- ggplot() + 
  geom_polygon(data = counties,
               aes(fill = dem_vote_prop,
                   x = x,
                   y = y,
                   group = group)) +
  geom_polygon(data = counties,
               aes(group = group,
                   x = x,
                   y = y),
               color = "black",
               fill = NA,
               size = .15) +
  geom_polygon(data = states,
               aes(group = group,
                   x = x,
                   y = y),
               color = "black",
               fill = NA,
               size = .25) +
  scale_fill_gradientn(breaks = breaks,
                       colors = c("red", 
                                  "#ff0040", 
                                  "purple", 
                                  "#4000ff", 
                                  "blue")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        legend.position = "None") +
  labs(title = paste("2020 Presidential Election as of", Sys.time(),
                     sep = " "),
       caption = "Only Republican and Democratic votes considered")