library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(transformr)

# Load data
energy <- readr::read_csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')
co2 <- readr::read_csv('https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv')

# Filter data
energy.2019 <- energy %>%
  filter(year == 2019) %>%  # Keep data for 2019
  select(country, iso_code, energy_per_capita) # Select the two columns of interest

library(sf)
library(giscoR)

co2.china <- co2 %>%
  filter(country == 'Japan' | country == 'Germany')

co2.china %>%
  ggplot() +
  geom_line(aes(x = year, y = co2, color = country))

co2.china %>%
  ggplot() +
  geom_line(aes(x = year, y = population, color = country))

co2.china %>%
  ggplot() +
  geom_line(aes(x = year, y = gdp/population, color = country))

#European countries
#https://epsg.io/3035
epsg_code <- 3035
EU_countries <- gisco_get_countries(region = "Europe") %>%
  st_transform(epsg_code)
EU_countries.energy <- left_join(EU_countries, energy.2019, by= c("ISO3_CODE" = "iso_code"))
ggplot(EU_countries.energy) +
  geom_sf(aes(fill = energy_per_capita)) +
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000)) +
  labs(
    title = "Energy per Capita in 2019",
    subtitle = "Europe",
    caption = paste0(
      "Source: OurWorldInData.org/energy/, ", gisco_attributions()
    )
  )

# Whole world
All_countries <- gisco_get_countries() 
All_countries.energy <- left_join(All_countries, energy.2019, by= c("ISO3_CODE" = "iso_code"))
ggplot(All_countries.energy) +
  geom_sf(aes(fill = energy_per_capita)) +
  labs(
    title = "Energy per Capita in 2019",
    subtitle = "World",
    caption = paste0(
      "Source: OurWorldInData.org/energy/, ", gisco_attributions()
    )
  )



#Gif Stuff

# Draw Map
ggplot(energy.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = energy_per_capita ), color = "white") +
  transition_time(year) +
  ease_aes('linear')

animate(p1)
anim_save('plot_gdpPercap_lifeExp.gif')

