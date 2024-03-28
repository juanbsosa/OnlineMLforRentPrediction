#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#   JOIN MONTHLY DATA INTO YEARLY DATA              #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(sf)
library(dplyr)


# Function -----------------------------------------------------------------

JoinYearlyFiles <- function(
        path_data_in="Data/Meli/Clean/Venta",
        max_rows=Inf,
        # assign=F,
        save=F,
        return=T,
        browser=F
){
    
    # List all files
    files <- list.files(path=path_data_in)
    
    # Load city limits
    path_ <- file.path(getwd(), "Data/CABAOpenData/caba", "caba.gpkg")
    caba <- st_read(path_)
    caba <- st_transform(caba,crs=5349)
    
    # Read files
    i=0
    for(i in 1:length(files)){
        
        # browser()
        
        print(paste("Reading the file", files[i], "..."))
        path_ <- file.path(getwd(), path_data_in, files[i])
        
        # Only some columns
        cols <- c(
            # dates
            "MesListing", "year", "semester", "quarter", "month",
            # id
            "ITE_ITEM_ID",
            # prices
            # "ITE_BASE_CURRENT_PRICE", "ITE_CURRENT_PRICE",
            # "SIT_CURRENCY_ID", 
            # "exp_in_dollars", # todas estan en usd
            "price_usd", 
            # "p_pesos", "p_real",
            # "pm2_covered", "pm2_tot", 
            "pm2_covered_usd", "pm2_tot_usd",
            # property type
            # "TIPOPROPIEDADNORM",
            "TIPOPROPIEDAD", 
            # "prop_type_norm",
            # area
            "STotalM2", "SConstrM2", "uncovered_m2",
            # amenities
            "Dormitorios", "Banos", "Ambientes", 
            # "Amoblado", # son todos NO, y el resto NA 
            "Antiguedad", 
            "Estacionamiento", 
            "Pileta",
            "Gimnasio",
            "Seguridad", 
            # "SalonFiestas",  "fiesta", 
            # "SalonDeUsosMul", 
            # "Sum", # todos 0
            # location
            "ITE_ADD_CITY_NAME",
            "ITE_ADD_STATE_NAME",
            "ITE_ADD_NEIGHBORHOOD_NAME", 
            "LONGITUDE",  "LATITUDE.Y",
            # Meli
            # "TIM_DAY", "ITE_AUCTION_START", "ITE_AUCTION_STOP",
            # Geo
            "link", "codpcia", 
            # "coddpto", 
            # "radio", 
            # "name_prov", 
            "name_dpto"
        )
        
        df_aux <- fread(path_, encoding = "UTF-8", 
                            nrows=max_rows, select=cols)
        rm(cols)
        
        # Filter by location
        df_aux <- df_aux[ITE_ADD_CITY_NAME == "CAPITAL FEDERAL" &
                     (ITE_ADD_STATE_NAME == "Capital Federal" |
                         ITE_ADD_STATE_NAME == "CAPITAL FEDERAL")]
        df_aux[, c("ITE_ADD_CITY_NAME", "ITE_ADD_STATE_NAME") := NULL]
        
        df_aux$name_dpto <- toupper(stringi::stri_trans_general(df_aux$name_dpto,"Latin-ASCII"))
        df_aux <- df_aux[grepl("COMUNA", name_dpto)]
        
        df_aux <- df_aux[codpcia==2]
        df_aux[, codpcia := NULL]
        
        df_aux <- st_as_sf(df_aux, coords = c("LONGITUDE", "LATITUDE.Y"), 
                           crs = 4326)
        df_aux <- st_transform(df_aux, crs=5349)
        df_aux <- df_aux[lengths(st_intersects(df_aux, caba)) > 0, ]
        
        # Drop geometry
        df_aux <- st_transform(df_aux, crs=4326)
        df_aux <- df_aux %>%
            mutate(LONGITUDE = st_coordinates(.)[,1],
                   LATITUDE = st_coordinates(.)[,2])
        df_aux <- st_drop_geometry(df_aux)
        
        # Filter by property type
        df_aux <- df_aux[TIPOPROPIEDAD %in% c("Departamento", "Casa", "PH")]
        
        # Delete properties with 0 bedrooms
        df_aux <- df_aux[Dormitorios>0]
        
        if(nrow(df_aux)==0){stop("Empty data.")}
        
        # Rename first df
        if(i==1){
            df <- df_aux
        } else{
            
            # Bind other dfs
            df <- rbind(df, df_aux)
            
        }
        
        rm(df_aux)
        gc()
        
        
    }
    
    rm(caba)
    gc()
    
    return(df)
    
} # end of function


AddVariables <- function(
    df,
    path_data_in="Data"
){
    
    # Keep only one observation per listing (randomly)
    print("Keeping only one observation per listing...")
    set.seed(789)
    df <- setDT(df)[, .SD[sample(.N, 1)], by = ITE_ITEM_ID]
    
    # Fix variables
    df[Gimnasio=="No", Gimnasio := "0"]
    df[Gimnasio=="Si", Gimnasio := "1"]
    df[Gimnasio=="Sí", Gimnasio := "1"]
    df$Gimnasio <- as.integer(df$Gimnasio)
    
    df[Seguridad=="No", Seguridad := "0"]
    df[Seguridad=="Si", Seguridad := "1"]
    df[Seguridad=="Sí", Seguridad := "1"]
    df$Seguridad <- as.integer(df$Seguridad)
    
    # Impute NAs
    df$Estacionamiento[is.na(df$Estacionamiento)] <- 0
    df$Pileta[is.na(df$Pileta)] <- 0
    df$Gimnasio[is.na(df$Gimnasio)] <- 0
    df$Seguridad[is.na(df$Seguridad)] <- 0
    
    gc()
    
    # Create dummies for property type
    df <- df[TIPOPROPIEDAD=="PH", TIPOPROPIEDAD := "Departamento"]
    df <- fastDummies::dummy_cols(df, select_columns = "TIPOPROPIEDAD")
    
    # Convert to sf data frame
    df <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    df <- st_transform(df, crs=5349)
    
    # Add neighbourhood variables
    path_ <- file.path(getwd(), path_data_in, "CABAOpenData", "barrios")
    neighs <- st_read(path_)
    neighs <- st_transform(neighs, crs=5349)
    df <- st_join(df, neighs[,"BARRIO"])
    rm(neighs)
    gc()
    
    # Add distance to nearest subway stop
    path_ <- file.path(getwd(), path_data_in, "CABAOpenData", "bocas-de-subte")
    subways <- st_read(path_)
    subways <- st_transform(subways, crs=5349)
    distances  <- st_distance(df, subways)
    df$dist_near_subway <- apply(distances, 1, min)
    rm(subways, distances)
    gc()
    
    # Green spaces
    path_ <- file.path(getwd(), path_data_in, "CABAOpenData", "espacio_verde_publico")
    greenspaces <- st_read(path_)
    greenspaces <- st_transform(greenspaces, crs=5349)
    # a <- greenspaces %>% st_drop_geometry() %>% group_by(clasificac) %>% summarise(avg_area=mean(area))
    # mapview::mapview(greenspaces[greenspaces$clasificac=="PARQUE", "area"])
    # a <- greenspaces[greenspaces$area<10000, "area"]
    # mapview::mapview(a)
    greenspaces <- greenspaces %>% filter(clasificac %in% 
        # c("PLAZA", "JARDÍN BOTÁNICO", "PARQUE", "PARQUE SEMIPÚBLICO") &
        c("PARQUE") &
            area > 7000)
    greenspaces <- st_union(greenspaces)
    # distances  <- st_distance(df, greenspaces)
        # m2 of greenspace in a 1km radius around each property
    # Create a buffer of 1 km around each point
    buffer_radius <- 1000  # 1 km in meters
    buffered_points_sf <- st_buffer(df, dist = buffer_radius)
    # Perform the intersection to get polygons within the buffer
    intersected_polygons_sf <- st_intersection(buffered_points_sf, greenspaces)
    # Calculate the area of the intersected polygons
    intersected_polygons_sf$area_within_1km <- as.numeric(st_area(intersected_polygons_sf))
    intersected_polygons_sf <- intersected_polygons_sf %>% select(ITE_ITEM_ID, area_within_1km)
    df <- left_join(df, st_drop_geometry(intersected_polygons_sf))
    
    # Drop geometry
    df <- st_transform(df, crs=4326)
    df <- df %>%
        mutate(LONGITUDE = st_coordinates(.)[,1],
               LATITUDE = st_coordinates(.)[,2])
    df <- st_drop_geometry(df)
    
    # Impute mean by neighbourhood for Antiguedad
    df <- setDT(df)[, mean_age := mean(Antiguedad, na.rm = TRUE), by = BARRIO]
    df$mean_age <- as.integer(df$mean_age)
    df <- df[is.na(Antiguedad), Antiguedad := mean_age]
    df$mean_age <- NULL
    
    # Renombrar variables
    cols <- list(
        ITE_ITEM_ID = "id",
        MesListing = "listing_month",
        year = "year",
        semester = "semester",
        quarter = "quarter",
        month = "month_number",
        price_usd = "price",
        pm2_covered_usd = "price_per_covered_m2",
        pm2_tot_usd = "price_per_m2",
        TIPOPROPIEDAD = "property_type",
        TIPOPROPIEDAD_Casa = "house",
        TIPOPROPIEDAD_Departamento = "flat",
        STotalM2 = "total_m2",
        SConstrM2 = "covered_m2",
        uncovered_m2 = "uncovered_m2",
        Dormitorios = "bedrooms",
        Banos = "bathrooms",
        Ambientes = "rooms",
        Antiguedad = "age",
        Estacionamiento = "parking",
        Pileta = "pool",
        Gimnasio = "gym",
        Seguridad = "security",
        BARRIO = "neighbourhood",
        dist_near_subway = "dist_near_subway",
        LONGITUDE = "longitude",
        LATITUDE = "latitude"
    )
    
    
    # Eliminar variables que ya no sirven
    sub <- names(cols)
    df2 <- df[, ..sub]
    
    # Agregar datos de amenities por ubicacion
    # Distancia a tren o subte mas cercano
    # Calcular distancia por calle, no euclidea
    
    return(df)
    
}