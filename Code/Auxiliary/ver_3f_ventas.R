library(sf)
library(mapview)
library(stringr)
library(raster)
library(rgdal)
library(gstat)

directory <- "G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Meli/1. Clean georef data"
extension <- ".gpkg"
keyword <- "Venta"

# Get a list of all files with the specified extension in the directory
files <- list.files(directory, pattern = extension, full.names = TRUE)

# Filter the files that contain the keyword in their names
matching_files <- files[str_detect(files, fixed(keyword))]

# Filter the files that contain the keyword in their names
matching_files <- matching_files[str_detect(matching_files, fixed("amba"))]

matching_files <- matching_files[-9]
matching_files <- matching_files[-9]


combined_df <- data.frame()
for(file in matching_files){

    aux <- st_read(file)
    # aux <- aux[!duplicated(aux$ITE_ITEM_ID),]
    aux <- aux[aux$name_dpto=="TRES DE FEBRERO",]
    combined_df <- rbind(combined_df, aux)
    rm(aux)
    gc()

}


df <- combined_df[!(duplicated(combined_df$ITE_ITEM_ID)),]
mapview(df)
st_write(df, dsn="I:/My Drive/Tesis Maestría/Datos/prueba3f.gpkg", delete_dsn = T, delete_layer = T)


df <- readOGR(dsn="I:/My Drive/Tesis Maestría/Datos/prueba3f.gpkg")
df <- sp::spTransform(df, CRS('+proj=longlat +datum=WGS84'))
# coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

# localidades <- st_read("C:/Users/Usuario/Documents/3F/Recaudacion/Datos/localidades/localidades.shp")
# muni <- sfheaders::sf_remove_holes(st_union(localidades))
# muni <- st_read("C:/Users/Usuario/Documents/3F/Recaudacion/Datos/limites3f/limites3f.shp")
polygon  <- readOGR(dsn="C:/Users/Usuario/Documents/3F/Recaudacion/Datos/limites3f/limites3f.shp")
polygon  <- sp::spTransform(polygon , CRS('+proj=longlat +datum=WGS84'))

df <- df[1:5000,]

variogram_model <- fit.variogram(variogram(ITE_BASE_CURRENT_PRICE ~ 1, data = df), model = vgm(4, "Exp", 5000, 1000))
kriging_result <- krige(ITE_BASE_CURRENT_PRICE ~ 1, df, polygon, model = variogram_model)

plot(kriging_result, main = "Kriging Result")

# bbox <- st_bbox(muni)
# raster_layer <- raster(bbox)
raster_layer <- raster(ext = extent(muni))

res(raster_layer) <- c(10, 10)
crs(raster_layer) <- crs(muni)

points <- st_coordinates(combined_df)  # Extract the coordinates of the polygon
values <- extract(raster_layer, points)  # Extract raster values at the polygon coordinates




# 
# a <- st_read("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Meli/1. Clean georef data/bases_amba_2022_Venta_2.gpkg")
# 
# a <- a[a$name_dpto=="TRES DE FEBRERO",]
# 
# a <- a[!duplicated(a$ITE_ITEM_ID),]
# 
# mapview(a)
# 
# 
# b <- st_read("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Meli/1. Clean georef data/bases_amba_2022_Venta_1.gpkg")
# 
# b <- b[b$name_dpto=="TRES DE FEBRERO",]
# 
# b <- b[!duplicated(b$ITE_ITEM_ID),]
# 
# mapview(b)



# asdas -------------------------------------------------------------------

rm(list=ls())
library(sf)
library(mapview)
library(stringr)
library(readr)

combined_df2 <- data.frame()
for(i in 2018:2022){
    
    path_ <- glue::glue("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Meli/0. Raw data/bases_amba_{i}_Venta.csv")
    print(i)
    aux <- read_csv(path_)
    # aux <- aux[!duplicated(aux$ITE_ITEM_ID),]
    aux <- aux[aux$ITE_ADD_CITY_NAME=="Tres de febrero",]
    combined_df2 <- rbind(combined_df2, aux)
    combined_df2$...1 <- NULL
    rm(aux)
    gc()
    
}

combined_df2$anio <- format(combined_df2$MesListing, "%Y")

combined_df2_unique <- combined_df2[!duplicated(combined_df2$ITE_ITEM_ID),]

write_csv(combined_df2, "I:/My Drive/Tesis Maestría/Datos/venta_3f_no_duplicados.csv")

lotes <- combined_df2[combined_df2$TIPOPROPIEDAD=="Terreno y lote",]
lotes <- lotes[!is.na(lotes$LATITUDE),]
lotes_sf <- st_as_sf(lotes, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview(lotes_sf)


# Alquileres -------------------------------------------------------------------

rm(list=ls())
library(sf)
library(mapview)
library(stringr)
library(readr)

keep_vars <- c(
    # dates
    "MesListing",
    # id
    "ITE_ITEM_ID",
    # prices
    "ITE_BASE_CURRENT_PRICE", "ITE_CURRENT_PRICE","ITE_SITE_CURRENT_PRICE",
    "SIT_CURRENCY_ID",
    # property type
    "TIPOPROPIEDADNORM","TIPOPROPIEDAD",
    # area
    "STotalM2", "SConstrM2",
    # amenities
    "Dormitorios", "Banos", "Ambientes", "Amoblado", "Antiguedad", 
    "Estacionamiento", "Pileta", "EstacionamientoVisitas",
    "BusinessCenter", "Gimnasio", "Laundry", "AreaJuegosInfantiles",
    "Seguridad", "PistaJogging", "SalonFiestas",  "SalonDeUsosMul",
    # location
    "ITE_ADD_CITY_NAME", "ITE_ADD_NEIGHBORHOOD_NAME", "ITE_ADD_STATE_NAME"
    # Meli
    # "TIM_DAY", "ITE_AUCTION_START", "ITE_AUCTION_STOP",
    # Census
    #
)

combined_df2 <- data.frame()
for(i in 2018:2022){
    
    path_ <- glue::glue("G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Meli/0. Raw data/bases_amba_{i}_Alquiler.csv")
    aux <- read_csv(path_, col_select = keep_vars)
    # aux <- aux[!duplicated(aux$ITE_ITEM_ID),]
    aux <- aux[aux$ITE_ADD_CITY_NAME=="Tres de febrero",]
    combined_df2 <- rbind(combined_df2, aux)
    rm(aux)
    gc()
    
}

combined_df22 <- combined_df2[!duplicated(combined_df2$ITE_ITEM_ID),]
write_csv(combined_df22, "I:/My Drive/Tesis Maestría/Datos/alquiler_3f_no_duplicados.csv")


lotes <- combined_df2[combined_df2$TIPOPROPIEDAD=="Terreno y lote",]
lotes <- lotes[!is.na(lotes$LATITUDE),]
lotes_sf <- st_as_sf(lotes, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mapview(lotes_sf)
