library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(tidyverse)
library(transformr)

# Load data
energy <- readr::read_csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')
co2 <- readr::read_csv('https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv')


library(sf)
library(giscoR)

EU_countries <- gisco_get_countries(region = "Europe") %>%
  st_transform(3035)

test <-co2 %>%
  filter(year == 2019) %>%
  select(iso_code, co2) %>%
  right_join(EU_countries, by = c("iso_code" = "ISO3_CODE"))

test %>% ggplot() +
  geom_sf(aes(geometry = geometry, fill = co2)) +
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000)) +
  labs(title = "CO₂ Emissionen in 2019 in Europa")


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

energy %>%
  filter(year == 2019) %>%  # Keep data for 2019
  select(country, iso_code, energy_per_capita) %>%
left_join(EU_countries, ., by= c("ISO3_CODE" = "iso_code")) %>%
ggplot() +
  geom_sf(aes(fill = energy_per_capita)) +
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000))

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
EU_countries <- gisco_get_countries(region = "Europe") %>%
  st_transform(3035)

co2 %>%
  filter(year > 1999) %>%
  select(iso_code, year, co2_per_capita) %>%
  left_join(EU_countries, ., by = c("ISO3_CODE" = "iso_code")) %>%
  ggplot() +
  geom_sf(aes(fill = co2_per_capita)) +
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000)) +
  labs(
    title = 'CO₂ Emissionen pro Einwohner in Europa', subtitle = '{frame_time}') +
  transition_time(as.integer(year)) +
  ease_aes('linear')



# Draw Map
ggplot(energy.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = energy_per_capita ), color = "white") +


animate(p1)
anim_save('plot_gdpPercap_lifeExp.gif')

