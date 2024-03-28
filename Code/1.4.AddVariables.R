#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#               ADD EXTRA VARIABLES                 #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(sf)
library(dplyr)
library(mapview)


# Function -----------------------------------------------------------------

add_variables <- function(
        df,
        year,
        verbose=T,
        # plot_checks=T,
        browse=F
){
    
    if(browse){browser()}
    
    # semester
    df$semester = lubridate::semester(df$MesListing)
    
    # quarter
    df$quarter = lubridate::quarter(df$MesListing)
    
    # month
    df$month = lubridate::month(df$MesListing)
    
    # SPATIAL INTERSECTIONS
    
    # Convert to sf data frame
    df <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    df <- st_transform(df, crs=5349)
    
    # Distance to nearest SUBWAY or TRAIN STOP
    
    # Download subway stops
    path_ <- file.path(getwd(), "Data", "CABAOpenData", "bocas-de-subte")
    subways <- st_read(path_)
    subways <- st_transform(subways, crs=5349)

    # Download train stops
    path_ <- file.path(getwd(), "Data", "Ammenities", "train_stations_CABA.gpkg")
    trains <- st_read(path_)
    trains <- st_transform(trains, crs=5349)
    # Convert the MULTIPPOINT geometries to POINT geometries
    trains <- st_cast(trains, to = "POINT")
    
    # Bind
    subways$id <- 1:nrow(subways)
    subways <- subways[,"id"]
    st_geometry(subways) <- "geometry"
    trains$id <- 1:nrow(trains)
    trains <- trains[,"id"]
    st_geometry(trains) <- "geometry"
    transport_stations <- rbind(subways, trains)
    rm(subways, trains)
    gc()
    
    # Add distance
    distances  <- st_distance(df, transport_stations)
    df$dist_transport <- apply(distances, 1, min)
    rm(transport_stations)
    gc()
    
    
    # Distance to nearest GREEN SPACE
    
    # Green spaces
    path_ <- file.path(getwd(), "Data", "CABAOpenData", "espacio_verde_publico")
    greenspaces <- st_read(path_) %>% select(nombre, clasificac, area)
    
    # Filter
    greenspaces <- greenspaces %>% filter(clasificac %in%
        c("PLAZA", "JARDÍN BOTÁNICO", "PARQUE", "PARQUE SEMIPÚBLICO") &
                                              area > 10000)
    greenspaces <- greenspaces %>% filter(!(nombre %in%
        c("Balneario Norte", "Bosque Alegre (Depvo. M. Belgrano)",
          "Parque de la Raza (Aeroparque)")))

    # Fix geometry
    greenspaces <- st_transform(greenspaces, crs=5349)
    # greenspaces <- nngeo::st_remove_holes(greenspaces, max_area = 10000)
    greenspaces <- st_simplify(greenspaces, dTolerance=5)
    # mapview(greenspaces)
    greenspaces <- st_union(greenspaces)
    
    # Add distance
    distances  <- st_distance(df, greenspaces)
    df$dist_greenspace <- apply(distances, 1, min)
    rm(greenspaces)
    gc()
    
    # Distance to CBD (Obelisco)
    cbd <- data.frame(
        lat=-34.603737524695575,
        long=-58.38156271629649
    )
    cbd <-  st_as_sf(cbd, coords = c("long", "lat"), crs = 4326)
    cbd <- st_transform(cbd, crs=5349)
    distances  <- st_distance(df, cbd)
    df$dist_cbd <- apply(distances, 1, min)
    
    
    # Drop geometry
    df <- st_transform(df, crs=4326)
    df <- df %>%
        mutate(LONGITUDE = st_coordinates(.)[,1],
               LATITUDE = st_coordinates(.)[,2])
    df <- st_drop_geometry(df)
    df <- setDT(df)
    
    # # Check for missing values
    # if(any(is.na(df))){stop("There are missing values in the data frame.")}
    
    # Rename all variables
    cols <- list(
        OPERACION = "listing_type",
        ITE_ITEM_ID = "id",
        MesListing = "listing_month",
        ListingAge = "listing_age",
        year = "year",
        semester = "semester",
        quarter = "quarter",
        month = "month",
        price_pesos_real = "price_realpesos",
        price_usd = "price_usd",
        pm2_covered_pesos_real = "price_realpesos_per_covered_sqm",
        pm2_covered_usd = "price_usd_per_covered_sqm",
        in_usd = "invoiced_in_usd",
        house = "house",
        STotalM2 = "total_area",
        SConstrM2 = "covered_area",
        uncovered_m2 = "uncovered_area",
        Dormitorios = "bedrooms",
        Banos = "bathrooms",
        Ambientes = "rooms",
        Antiguedad = "property_age",
        Pileta = "pool",
        Seguridad = "security",
        Amoblado = "furnished",
        Calefaccion = "heating",
        AireAC = "air_conditioning",
        Parking = "parking",
        CommonArea = "common_space",
        FitnessArea = "fitness_space",
        commune = "commune",
        neighborhood = "neighbourhood",
        dist_transport = "distance_to_transport",
        dist_greenspace = "distance_to_greenspace",
        dist_cbd = "distance_to_cbd",
        LONGITUDE = "longitude",
        LATITUDE = "latitude"
    )
    sub <- names(cols)
    df <- df[, ..sub]
    rm(sub)
    setnames(df, old = names(cols), new = unlist(cols))

    
    print(glue::glue("Finished: Add Variables for year {year}"))
    cat('\n')
    return(df)
    
}