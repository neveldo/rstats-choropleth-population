# Choropleth map of France of population by city
# The map have to be downloaded from https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/ and
# extracted into the working directory
# @doc https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles

library(tidyverse)
library(stringr)
library(ggmap)
require(rgdal) 
require(maptools)
require(gpclib)
require(plyr)

# @see https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
gpclibPermit()

# Set up the population data
# @source https://www.data.gouv.fr/fr/datasets/population/
populationByCity <- read_csv('population2014.csv') %>%
  mutate(insee = str_c(`Code département`, `Code commune`)) %>%
  mutate(population = as.double(str_replace(`Population totale`, '\\s', ''))) %>%
  select(insee, population)

quartiles <- quantile(populationByCity$population)

populationByCity <- mutate(populationByCity, slice = cut(population, quartiles)) %>%
  filter(!is.na(slice)) %>%
  mutate(slice = factor(slice, labels = c('Moins de 202 habs', 'Entre 202 et 455 habs', 'Entre 455 et 1143 habs', 'Plus de 1143 habs')))

# Set up the map data
# @source https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
franceCities <- readOGR(dsn=".", layer="communes-20170112")
franceCities@data$id <- rownames(franceCities@data)
franceCities.points <- fortify(franceCities, region="id")
franceCities.df <- join(franceCities.points, franceCities@data, by="id")

franceCities.df <- left_join(franceCities.df, populationByCity)

# Restrict the map to metropolitan France
franceCities.df <- filter(franceCities.df, lat > 40.612089 & lat < 51.122672 & long > -7.917282 & long < 10.891311)

ggplot(franceCities.df, aes(long, lat, group = insee, fill = slice) ) + 
  geom_polygon() +
  coord_equal() +
  labs(
    title = "Nombre d'habitants par commune en France",
    fill = ""
  ) +
  scale_fill_brewer(palette = "OrRd", na.value = "#bababa") +
  theme_void() +
  theme(legend.position="bottom")