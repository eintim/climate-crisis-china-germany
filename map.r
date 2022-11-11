library(ggplot2)
library(dplyr)

# Load data
energy <- readr::read_csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')

# Load map data
world_map <- map_data("world")

# Filter data 2021
energy.2021 <- filter(energy, year == 2021)

# Merge data
energy_map.2021 <- left_join(world_map, energy.2021, by= c("region" = "country"))

# Draw Map
ggplot(energy_map.2021, aes(long, lat, group = group))+
  geom_polygon(aes(fill = energy_per_capita ), color = "white")