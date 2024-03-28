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

library(zoo)

infla$trimestral <- c(NA,NA,rollapply(infla[,3], 3, FUN = prod))
infla$anual <- c(rep(NA,11),rollapply(infla[,3], 12, FUN = prod))

infla$trimestral_1 <- c(NA,NA,NA,infla$trimestral[1:(nrow(infla)-3)])

infla <- infla[12:nrow(infla),]

infla$mes <- as.Date(paste0(infla$mes,'-01') , "%Y-%m-%d")

infla$quarter = paste0(lubridate::year(infla$mes),"-", lubridate::quarter(infla$mes))

infla_q <- infla %>%
  group_by(quarter) %>%
  summarise(
    infla_q = mean(trimestral),
    infla_q_1 =mean(trimestral_1),
    infla_a = mean(anual)
  )

infla_q <- infla_q[substr(infla_q$quarter,1,4) %in% c('2017','2018','2019','2020','2021','2022'),]

write.csv(infla_q,'Extras/Inflacion/infla_obs.csv')


