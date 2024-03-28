rm(list=ls())
gc()

library(readr)
library(readxl)
library(dplyr)


# Check the ReadMe file to see where this .xls file was obtained.
# For the next iteration of this process, the name of the file will be
# different because it depends on the current date
df_path <- "G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Extras/Inflacion/sh_ipc_01_23.xls"
df <- read_excel(df_path,
                 col_names = FALSE, skip = 5,
                 sheet="Índices IPC Cobertura Nacional")


# IPC TOTAL NACIONAL

# Create new fata frame with month, index, and inflation rate
dates <- as.Date(as.integer(df[1,2:ncol(df)]), origin = "1899-12-30")
index <- as.numeric(df[5,2:ncol(df)]) # row 5 is for "Nivel general" (in case the row number changes in a new version of the file)

infla <- data.frame(month = dates,
                    index = index)
infla <- infla %>% mutate(rate = (index - lag(index))/ lag(index) * 100)

write.csv(infla, "G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Extras/Inflacion/infla_nacional_INDEC.csv", row.names = FALSE)


# IPC GBA
dates <- as.Date(as.integer(df[1,2:ncol(df)]), origin = "1899-12-30")
index <- as.numeric(df[35,2:ncol(df)]) # row 35 is "Nivel general"

infla_gba <- data.frame(month = dates,
                    index = index)
infla_gba <- infla_gba %>% mutate(rate = (index - lag(index))/ lag(index) * 100)


# Infla BCRA (originalarchivo original en trabajo de Santi)
Inflacion_BCRA <- read_csv("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Extras/Inflacion/Inflacion BCRA.csv")
colnames(Inflacion_BCRA) <- c('fecha','rate0','index')
Inflacion_BCRA$fecha <- as.Date(Inflacion_BCRA$fecha,"%d/%m/%Y")
Inflacion_BCRA$month <- as.Date(format(Inflacion_BCRA$fecha,"%Y-%m-01"))
Inflacion_BCRA <- Inflacion_BCRA[,c('month','index')]
Inflacion_BCRA <- Inflacion_BCRA %>% mutate(rate = (index - lag(index))/ lag(index) * 100)


# Compare the inflation rates
inflas <- merge(infla, infla_gba, by='month', suffixes = c(".nac",".gba"))
inflas <- left_join(inflas, Inflacion_BCRA, by='month')
colnames(inflas)[which(colnames(inflas) %in%  c("index", "rate"))] <- c("index.bcra", "rate.bcra")

plot(inflas$month, inflas$rate.nac, type='l', col=scales::alpha("blue", 0.8), lwd=2)
lines(inflas$month, inflas$rate.gba, col=scales::alpha("red", 0.8), lwd=2)
lines(inflas$month, inflas$rate.bcra, col=scales::alpha("green", 0.8), lwd=2)
legend(x = "topleft", box.col = "brown",
       bg ="yellow", box.lwd = 2 , title="Indexes", 
       legend=c("Total Nacional", "GBA", "BCRA"), 
       fill = c(scales::alpha("blue", 0.8),scales::alpha("red", 0.8), scales::alpha("green", 0.8)))

