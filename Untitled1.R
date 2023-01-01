library(tidyverse)

energy <- readr::read_csv('https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv')
co2 <- readr::read_csv('https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv')

co2_filtered <- co2 %>%
  filter(country == "Germany") %>%
  select(country, year, co2, coal_co2, cement_co2, flaring_co2, gas_co2, oil_co2)
  
ggplot(co2_filtered) +
  geom_line(aes(x = year, y = co2), color = co2)+
  labs(
    title = "Jährlicher Ausstoß von CO₂ in China und Deutschland", 
    x = "Jahr",
    y = "CO₂ in Millionen Tonnen"
  )

ggplot(co2_filtered) +
  #geom_line(aes(x = year, y = co2/co2, color = 'CO2')) +
  geom_line(aes(x = year, y = coal_co2/co2, color = 'Kohle')) +
  geom_line(aes(x = year, y = cement_co2/co2, color = 'Zement')) +
  geom_line(aes(x = year, y = flaring_co2/co2, color = 'Gasfackeln')) +
  geom_line(aes(x = year, y = gas_co2/co2, color = 'Gas')) +
  geom_line(aes(x = year, y = oil_co2/co2, color = 'Öl')) +
  expand_limits(y = c(0, 1)) +
  scale_color_manual(
    name = "Fossiler Energieträger",
    breaks = c('CO2', "Kohle", "Zement", "Gasfackeln", "Gas", "Öl"),
    values = c("CO2" = "black", "Kohle" = "#f8766d", "Zement" = "#a3a500", "Gasfackeln" = "#00bf7d", "Gas" = "#00b0f6", "Öl" = "#e76bf3")
    ) +
  labs(title = NULL, x = 'Jahr', y = 'Anteil der fossilen Energieträger an CO₂ Emissionen')



