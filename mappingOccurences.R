##### Mapping Bee Data ----

#install.packages("leaflet")
library(leaflet)
library(tidyverse)
beeRaw<- read_csv("BeeOcc.csv") %>% 
  filter(!is.na(locality)&!is.na(startYear))

beeRaw%>%
  filter(!is.na(Species)) %>% 
  unite(beeName, Genus:Species, sep = " ") %>% 
  filter(!is.na(florelation)) %>% 
  unite(interaction, beeName, florelation, sep = " - ")

leaflet(beeData) %>% 
  addTiles() %>% 
  addMarkers(lng = ~lonTrap, lat = ~latTrap, popup = beeData$interaction, clusterOptions = markerClusterOptions())
