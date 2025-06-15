# Load libraries.
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(munsell)

# Load data.
load("data/controlNGA.rdata")
load("data/intensityNGA.rdata")
load("data/locationsNGA.rdata")
load("data/eventsNGA.rdata")
load("data/regionsNGA.rdata")

meanControlNGA <- apply(controlNGA, 2:3, mean)
meanIntensityNGA <- apply(intensityNGA, 2:3, mean)

rebselEvents <- eventsNGA %>% filter(type == "Rebel Selective")
rebindEvents <- eventsNGA %>% filter(type == "Rebel Indiscriminate")
rebconEvents <- eventsNGA %>% filter(type == "Rebel Conventional")
battleEvents <- eventsNGA %>% filter(type == "Battle")
govconEvents <- eventsNGA %>% filter(type == "Government Conventional")
govindEvents <- eventsNGA %>% filter(type == "Government Indiscriminate")
govselEvents <- eventsNGA %>% filter(type == "Government Selective")
