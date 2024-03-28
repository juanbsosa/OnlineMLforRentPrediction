#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#           CLEAN DATA BY LOCATION                  #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(sf)
library(mapview)
library(dplyr)
library(stringr)


# Function -----------------------------------------------------------------

clean_data_geo <- function(
        df,
        year,
        verbose=T,
        plot_checks=T,
        browse=F
){
    
    if(browse){browser()}
    
    # Report initial observations
    n_rows_start <- nrow(df)
    print(glue::glue("Data for year {year}: {n_rows_start} observations before any cleaning by location."))
    cat("\n")
    
    # Load polygon for CABA
    path_ <- file.path(getwd(), "Data", "Geo", "CABA", "CABA.gpkg")
    caba <- st_read(path_)
    caba <- st_transform(caba, crs=4326)

    # 1) FILTER BY BOUNDING BOX
    caba_bbox <- st_bbox(caba)
    old_n <- nrow(df)
    df <- subset(df, LONGITUDE > caba_bbox[1] & LONGITUDE < caba_bbox[3] &
                        LATITUDE > caba_bbox[2] & LATITUDE < caba_bbox[4])
    # Report
    if(verbose){
        print(paste(old_n-nrow(df), "rows removed because location is outside bouding box."))
        cat("\n")
    }
    
    # 2) FILTER BY INTERSECTION WITH CITY POLYGON
    
    # Convert df to sf
    df <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    
    # Change CRS
    # posgar 2007/7 https://epsg.io/5349
    df <- st_transform(df, crs=5349)
    caba <- st_transform(caba, crs=5349)
    
    old_n <- nrow(df)
    df <- df[lengths(st_intersects(df, caba)) > 0, ]
    
    # Report
    if(verbose){
        print(paste(old_n-nrow(df), "rows removed because location does not intersect with shapefile."))
        cat("\n")
    }
    
    
    # 3) FILTER BY CONSISTENCY OF LOCATION VARIABLES
    
    # Remove all double spaces
    df$ITE_ADD_NEIGHBORHOOD_NAME <- gsub("  ", " ", df$ITE_ADD_NEIGHBORHOOD_NAME)
    
    # Fix some neighbourhoods
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME==""] <- NA
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="BUENOS AIRES"] <- NA
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="ARGENTINA"] <- NA
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="LAS CANITA"] <- "LAS CANITAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="LAS CA?ITAS"] <- "LAS CANITAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="CONSTITUCI&OACUTE;N"] <- "CONSTITUCION"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="NU?EZ"] <- "NUNEZ"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="N&UACUTE;&NTILDE;EZ"] <- "NUNEZ"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="BARRIO NORTE"] <- "RECOLETA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="BARIO NORTE"] <- "RECOLETA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="RECOLEDA"] <- "RECOLETA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="RECOLETOS"] <- "RECOLETA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="R4COLETA"] <- "RECOLETA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="CENTRO / MICROCENTRO"] <- "SAN NICOLAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="CENTRO"] <- "SAN NICOLAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="CENTO"] <- "SAN NICOLAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="SAN NICOL&AACUTE;"] <- "SAN NICOLAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="SAN NICOL&AACUTE;S"] <- "SAN NICOLAS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="ABASTO"] <- "ALMAGRO"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="BALBANERA"] <- "BALVANERA"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="MOSERRAT"] <- "MONTSERRAT"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="MONSERRAT"] <- "MONTSERRAT"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="PARQUE PATRICIO"] <- "PARQUE PATRICIOS"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="RITIRO"] <- "RETIRO"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="PUERTO RETIRO"] <- "RETIRO"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_NEIGHBORHOOD_NAME=="VILLA GENERAL MITRE, CABA,"] <- "VILLA GENERAL MITRE"
    df$ITE_ADD_NEIGHBORHOOD_NAME[df$ITE_ADD_CITY_NAME=="BARRACAS"] <- "BARRACAS"
    
    # Load mapping of neighbourhoods to communes
    path_ <- file.path(getwd(), "Code", "Auxiliary", "caba_location_mapping.R")
    source(path_)
    dpto_mapping_caba <- return_list()

    # Create dictionary mapping neighbourhood names to department names
    neigh_to_dept <- stack(dpto_mapping_caba)
    rm(dpto_mapping_caba)
    names(neigh_to_dept) <- c("neigh", "dept")
    neigh_to_dept$dept <- str_replace_all(toupper(neigh_to_dept$dept), "_", " ")
    
    # Use dictionary to map neighborhood names to department names in df
    df$neigh_norm <- neigh_to_dept$dept[match(df$ITE_ADD_NEIGHBORHOOD_NAME, 
                                              neigh_to_dept$neigh)]
    rm(neigh_to_dept)
    gc()
    
    # Print all neighbourhoods that didn't match
    if(verbose){
        print("These neighbourhoods did not match with the mapping:")
        print(unique(df$ITE_ADD_NEIGHBORHOOD_NAME[is.na(df$neigh_norm)]))
        cat("\n")
    }
    
    # Flag those that did not match and are not missing values
    df$neigh_norm[is.na(df$neigh_norm)] <- ifelse(
        is.na(df$ITE_ADD_NEIGHBORHOOD_NAME[is.na(df$neigh_norm)]), 
        NA, "NoMatch")
    
    # Load neighbourhood polygons
    path_ <- file.path(getwd(), "Data", "CABAOpenData", "barrios", 
                       "barrios_wgs84.shp")
    neighs <- st_read(path_) %>% select(-c(PERIMETRO, AREA, OBJETO))
    neighs <- st_transform(neighs, crs=5349)
    neighs$BARRIO <- stringi::stri_trans_general(neighs$BARRIO,"Latin-ASCII")
    neighs$COMUNA <- paste("COMUNA", neighs$COMUNA)
    
    # Find contiguous communes
    # Read shapefile
    path_ <- file.path(getwd(), "Data", "CABAOpenData", "Communes_CABA", "comunas_wgs84.shp")
    communes <- st_read(path_)
    communes <- communes %>% select(COMUNAS) %>% rename(COMUNA = COMUNAS)
    communes <- st_transform(communes, crs=5349)
    
    # Create column with contigous communes
    sf_use_s2(FALSE)
    communes$contiguous <- sapply(st_touches(communes), function(x) {
        paste(st_drop_geometry(communes[x, "COMUNA"]), collapse = ", ")
    })
    sf_use_s2(TRUE)
    communes$contiguous <- gsub("^c\\(|\\)$", "", as.character(communes$contiguous))
    communes$contiguous <- gsub("(\\d+)", "COMUNA \\1", communes$contiguous)
    communes$COMUNA <- gsub("(\\d+)", "COMUNA \\1", communes$COMUNA)

    # Add to neighborhood polygons
    neighs <- left_join(neighs, st_drop_geometry(communes))
    rm(communes)
    gc()
    
    # Intersect df with neighborhood polygons
    neighs <- neighs %>% rename(commune=COMUNA, neighborhood=BARRIO)
    old_n <- nrow(df)
    df <- st_intersection(df, neighs)
    rm(neighs)
    gc()

    # Report
    if(verbose){
        print(paste(old_n-nrow(df), "rows removed because location does not intersect with neighborhoods polygon."))
        cat("\n")
    }
    
    # Create validation variable that takes value TRUE if normalized location matches
    #   census localion, false if it doesnt, and NA if normalized location is missing
    df <- df %>% mutate(validate = case_when(
        
        df$neigh_norm == df$commune & 
            !is.na(df$neigh_norm) & !is.na(df$commune) ~ TRUE,
        
        df$neigh_norm != df$commune & 
            !is.na(df$neigh_norm) & !is.na(df$commune) ~ FALSE,
        
        TRUE ~ NA
    )
    )
    # Also allow if the commune in the df is contiguos to the correct one
    df$aux <- sapply(seq_along(df$neigh_norm), 
                     function(i) grepl(df$neigh_norm[i], df$contiguous[i])) 
    df$validate <- ifelse(df$aux, TRUE, df$validate)
    df <- df %>% select(-c("aux", "contiguous"))
    
    # Delete observations where "validate" equals FALSE
    old_n <- nrow(df)
    df <- df[df$validate==TRUE | is.na(df$validate), ]
    df <- df %>% select(-c("validate", "neigh_norm"))
    if(verbose){
        print(paste(old_n-nrow(df), 
                    "rows removed because commune name does not coincide with actual location."))
        cat("\n")
        
        # Print all neighbourhoods that didn't match
        if(verbose){
            print("These are the remaining neighbourhoods:")
            print(unique(df$ITE_ADD_NEIGHBORHOOD_NAME))
            cat("\n")
        }
        
        print(glue::glue("Data for year {year}: {n_rows_start} observations before any cleaning. {nrow(df)} left after cleaning by location."))
        cat("\n")
        
    }
    
    # Check for duplicates
    print(paste("Are there any duplicates in terms of month and ID?:", any(duplicated(cbind(df$MesListing, df$ITE_ITEM_ID)))))
    cat("\n")
    
    # Remove geometry
    df <- st_transform(df, crs=4326)
    df <- df %>%
        mutate(LONGITUDE = st_coordinates(.)[,1],
               LATITUDE = st_coordinates(.)[,2])
    df <- setDT(st_drop_geometry(df))
    
    # Remove variables
    df$ITE_ADD_CITY_NAME <- NULL
    df$ITE_ADD_NEIGHBORHOOD_NAME <- NULL
    
    
    if(plot_checks){
        # Plot the evolution of the number of listings, just to check if any month is missing
        print("Creating plot of the number of listings by month...")
        cat("\n")
        plot <- ggplot(df[, .(Observations = .N), by = MesListing], 
                       aes(x = MesListing, y = Observations, group = 1)) +
            geom_line() +
            geom_point() +
            scale_y_continuous(limits = c(0, 150000)) +
            labs(x = "Month", y = "Number of Observations", title = "Observations by Month")
        
        # Save plot
        path_ <- file.path(getwd(), "Output")
        directories <- c("Plots", "DataCleaning", "CleanDataByLocation")
        for (dir_name in directories) {
            path_ <- file.path(path_, dir_name)
            if (!dir.exists(path_)) {
                dir.create(path_)
            }
        }
        ggsave(file.path(path_, glue::glue("n_listings_{year}.png")), 
               plot, width = 8, height = 6, dpi = 300)
    }
    
    
    print(glue::glue("Finished: Clean by Location for year {year}"))
    cat('\n')
    return(df)
    
}
    
    