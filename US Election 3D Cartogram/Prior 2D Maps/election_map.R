library(tidyverse)
library(usmap)

# PICK WHAT YEAR YOU WANT TO PLOT
#################################

election_year <- 2012

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
  mutate(votes_total = sum(candidatevotes)) %>%
  ungroup %>%
  filter(party == "democrat") %>%
  group_by(year, FIPS) %>%
  mutate(dem_vote_prop = candidatevotes / votes_total) %>%
  ungroup()

# Create base map data
counties <- us_map("counties")
states <- us_map("states")
counties$fips <- as.integer(counties$fips)

# Join vote data to map data
counties <- counties %>%
  left_join(votes %>%
              select(FIPS, dem_vote_prop),
            by = c("fips" = "FIPS"))

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
  scale_fill_gradient(low = "red",
                       high = "blue") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        legend.position = "None") +
  labs(title = paste(election_year, 
                     "Presidential Election", 
                     sep = " "),
       caption = "Only Republican and Democratic votes considered")