rm(list=ls())

# library(sf)
library(dplyr)
# library(sp)
# library(spdep)
# library(doParallel)
# library(raster)
# library(spatialEco)
# library(Matrix)
# library(tmap)
# library(tmaptools)
# library(mapview)
# library(doParallel)
# library(spex)
# library(measurements)
library(readxl)
# library(OpenStreetMap)
# library("geosphere")

setwd("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data")

# INFLACION

infla <- read_csv('Extras/Inflacion/infla_nacional_INDEC.csv')

names(infla) <-c('mes','indice','var')

meses <- format(seq(as.Date('2016-12-01'),as.Date('2022-12-01'),by='1 month'),"%Y-%m")

infla<-as.data.frame(infla)

infla[,1] <- meses
infla[,3] <- 1+(as.numeric(infla[,'var'])/100)

write.csv(infla,'Extras/Inflacion/infla_obs_mes.csv')


