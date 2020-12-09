library(tidyverse)
library(usmap)
# If on Linux, may need to run `sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev`
library(tidycensus)
library(tigris)
library(maptools)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# ENTER CENSUS API KEY
#################################

# If you do not have a Censu API key get one here:
# http://api.census.gov/data/key_signup.html

# You should only need to do this once

# census_api_key("YOUR KEY HERE", install = TRUE)

# PICK WHAT STATE YOU WANT TO PLOT
#################################

state_name <- "Missouri"

# Download and read in vote data
temp <- tempfile()

vote_data_url <- "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/LYWX3D/1685d8cf12b-3c57b27160df?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27presidential_precincts_2016.rda&response-content-type=application%2Fx-rlang-transport&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20201105T215518Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20201105%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=39db2de1269ede253e209733adef23714a1cfa9c994d71da0009fdf119d8079a"

download.file(vote_data_url, temp, method = "wget", quiet = TRUE)

load(temp)

votes <- df1

rm(df1)

votes <- votes %>%
  filter(state == state_name)

# Create vote proportions
votes <- votes %>%
  filter(party %in% c("democratic", "republican")) %>%
  group_by(precinct) %>%
  mutate(votes_total = sum(votes)) %>%
  ungroup %>%
  filter(party == "democratic") %>%
  group_by(precinct) %>%
  mutate(dem_vote_prop = votes / votes_total) %>%
  ungroup()

# Create base map data
state <- voting_districts("Missouri")

ggplot(state) +
  geom_sf()

# Join vote data to map data
votes$precinct <- tolower(votes$precinct)
state$NAME10 <- tolower(state$NAME10)

precincts <- state %>%
  left_join(votes %>%
              select(precinct, dem_vote_prop),
            by = c("NAME10" = "precinct"))

precinct_map <- ggplot() + 
  geom_sf(data = precincts,
               aes(fill = dem_vote_prop)) +
  scale_fill_gradient(low = "red",
                      high = "blue") +
  theme_void() +
  theme(legend.position = "None")