library(ggplot2)
library(dplyr)

# Load data
energy <- readr::read_csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')
co2 <- readr::read_csv('https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv')

# Load map data
world_map <- map_data("world")

# Filter data
energy.2021 <- energy %>%
  filter(year == 2021) %>%  # Keep data for 2021
  select(country, energy_per_capita) %>%  # Select the two columns of interest
  rename(region = country) %>%  # Rename column
  # Replace "United States of America" by USA in the region column
  mutate(
    region = ifelse(region == "United States", "USA", region),
    region = ifelse(region == "United Kingdom", "UK", region),
    region = ifelse(region == "Congo", "Republic of Congo", region),
    region = ifelse(region == "Czechia", "Czech Republic", region)
    #region = ifelse(region == "????", "Democratic Republic of the Congo", region),
    #region = ifelse(region == "?????", "?????", region) #Elfenbeinküste
  )  

# Merge data
energy_map.2021 <- left_join(world_map, energy.2021, by= c("region"))

# Draw Map
ggplot(energy_map.2021, aes(long, lat, group = group))+
  geom_polygon(aes(fill = energy_per_capita ), color = "white")

