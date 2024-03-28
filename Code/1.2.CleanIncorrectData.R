#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#        CLEAN DATA BY VARIABLE VALUES              #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(ggplot2)

# To-Dos:
# - Count and report all deleted observations acc to the diff criteria
# - Plots have to be made by OPERACION


# Function -----------------------------------------------------------------

clean_data <- function(
        df,
        year,
        verbose=T,
        plot_checks=T,
        operations=c("ALQUILER"),
        browse=F
){
    
    if(browse){browser()}
    
    # Report initial number of rows
    n_rows_start <- nrow(df)
    if(verbose){
        print(glue::glue("{n_rows_start} observations before any further cleaning."))
        cat("\n")
    }

    
    # FILTERS
    
    # Normalize OPERACION
    df$OPERACION <- toupper(stringi::stri_trans_general(df$OPERACION,"Latin-ASCII"))
    df <- df[OPERACION=="ALQUILER TEMPORARIO", OPERACION := "ALQUILER TEMPORAL"]
    if(verbose){
        print(glue::glue("Year {year}: Check unique operations:"))
        print(unique(df$OPERACION))
        cat('\n')
    }
    # Keep only ALQUILER and VENTA
    print(glue::glue('Keeping only listing types: {operations}'))
    df <- df[OPERACION %in% operations]
    
    # Only in CABA
    if(verbose){
        print("Preliminary filter: unique values of variable STATE:")
        print(unique(df$ITE_ADD_STATE_NAME))
        print('Keeping only: "Capital Federal"')
        cat("\n")
        }
    df <- df[ITE_ADD_STATE_NAME == "Capital Federal"] # !!! hope that this is always lower case
    df$ITE_ADD_STATE_NAME <- NULL
    
    # Remove observations without coordinates
    df <- df[!is.na(LATITUDE) & !is.na(LONGITUDE)]
    
    # Non-negative price
    df <- df[ITE_BASE_CURRENT_PRICE>0 & ITE_CURRENT_PRICE>0]

    # Coherent m2 (non-negative total and built m2, total>=built)
    df <- df[STotalM2>0 & SConstrM2>0 & STotalM2>=SConstrM2]
    
    # Only keep residential properties
    # Property type: departamento o casa
    df$TIPOPROPIEDAD <- toupper(stringi::stri_trans_general(df$TIPOPROPIEDAD,"Latin-ASCII"))
    
    # replace Edificio and Duplex with Departamento
    df <- df[TIPOPROPIEDAD %in% c("EDIFICIO", "DUPLEX", "PH", "TRIPLEX"), 
             TIPOPROPIEDAD := "DEPARTAMENTO"]
    
    # "casa sola" = casa
    df <- df[TIPOPROPIEDAD == "CASA SOLA", 
             TIPOPROPIEDAD := "CASA"]
    
    if(verbose){
        print("Preliminary filter: unique values of variable PROPERTY TYPE:")
        print(unique(df$TIPOPROPIEDAD))
        print('Keeping only: Edificio, Departamento, Ph, Duplex, Casa, Casa Sola, Triplex')
        cat("\n")
    }
    
    # Flat or house
    df <- df[TIPOPROPIEDAD %in% c("DEPARTAMENTO", "CASA")]
    
    # House variable
    df <- df[, house := as.integer(TIPOPROPIEDAD=="CASA")]
    df$TIPOPROPIEDAD <- NULL
    
    # Minimum m2, for houses and flats
    df <- df[(house==0 & SConstrM2 >= 25) | house!=0]
    df <- df[(house==1 & SConstrM2 >= 35) | house!=1]
    
    # Maximum m2, for houses and flats
    df <- df[(house==0 & SConstrM2 <= 1500) | house!=0]
    df <- df[(house==1 & SConstrM2 <= 3000) | house!=1]
    
    # Max Relationship between uncovered m2 and covered m2
    # df$rel <- df$uncovered_m2 / df$SConstrM2
    df$uncovered_m2 <- df$STotalM2 - df$SConstrM2
    df <- df[uncovered_m2/SConstrM2<=3]

    # LISTING AGE: less than 1 year
    df <- df[, ListingAge := (MesListing+31) - ITE_AUCTION_START]
    df$ITE_AUCTION_START <- NULL
    df <- df[ListingAge<365]
    
    # Wrong values (very specific repetition of 1s) for some vars
    cols <- c("ITE_CURRENT_PRICE", "SConstrM2", "STotalM2")
    for(col in cols){
        df <- df[get(col)!=11111111]
        df <- df[get(col)!=1111111]
        df <- df[get(col)!=111111]
        df <- df[get(col)!=11111]
        df <- df[get(col)!=1111]
    }
    rm(cols, col)
    gc()
    
    # Bedrooms: between 1 and 10
    # df$Dormitorios[df$Dormitorios == "Más de 4"] <-  "5"
    df$Dormitorios <- as.numeric(df$Dormitorios)
    df <- df[Dormitorios >= 1 & Dormitorios <= 10]
    
    # Bathrooms: between 1 and 10
    # df$Banos[df$Banos == "Más de 4"] <-  "5"
    df$Banos <- as.numeric(df$Banos)
    df <- df[Banos >= 1 & Banos <= 10]
    
    # Rooms: between 1 and 10
    df$Ambientes <- as.numeric(df$Ambientes)
    df <- df[Ambientes >= 1 & Ambientes <= 15]
    
    # Rooms >= Bedrooms
    df <- df[Ambientes >= Dormitorios]

    
    # AGE
    # Missing values:
    df$Antiguedad[df$Antiguedad == "0"] <- NA
    # Deal with approximate values
    df$Antiguedad[df$Antiguedad == "A estrenar"] <- 0
    df$Antiguedad[df$Antiguedad == "Hasta 5"] <-  5
    df$Antiguedad[df$Antiguedad == "Hasta 10 años"] <- 10
    df$Antiguedad[df$Antiguedad == "Hasta 20 años"] <- 20
    df$Antiguedad[df$Antiguedad == "Hasta 30 años"] <- 30
    df$Antiguedad[df$Antiguedad == "Hasta 40 años"] <- 40
    df$Antiguedad[df$Antiguedad == "Hasta 50 años"] <- 50
    df$Antiguedad[df$Antiguedad == "Max 5 años"] <- 5
    df$Antiguedad[df$Antiguedad == "Más de 40 años"] <- 41
    # Extract numeric values from character
    df$Antiguedad <- readr::parse_number(df$Antiguedad)
    # Round to 0 decimal numbers
    df$Antiguedad <- round(df$Antiguedad, 0)
    # Deal with cases where value equals construction year: actual year minus value
    # create year
    df$year <- lubridate::year(df$MesListing)
    # fix
    df$Antiguedad[df$Antiguedad > 1800 & is.na(df$Antiguedad)==FALSE] <- ( df$year[df$Antiguedad > 1800 & is.na(df$Antiguedad)==FALSE] - df$Antiguedad[df$Antiguedad > 1800 & is.na(df$Antiguedad)==FALSE] )
    
    # Filter according to age
    df <- df[(Antiguedad >= 0 & Antiguedad <= 100) | is.na(Antiguedad)==TRUE]
    
    
    # Currency: delete blank spaces
    df$SIT_CURRENCY_ID = gsub(" ", "", df$SIT_CURRENCY_ID)
    df$SIT_CURRENCY_ID[df$SIT_CURRENCY_ID=="N/I"] <- NA
    
    # For May/June/July 2022, there is a problem with the prices and the currency
    if(year==2022){
        
        df$ITE_BASE_CURRENT_PRICE[
            df$MesListing %in% c("2022-05-01","2022-06-01","2022-07-01") & 
                df$in_usd==1] <- 
            df$ITE_CURRENT_PRICE[
                df$MesListing %in% c("2022-05-01","2022-06-01","2022-07-01") & 
                    df$in_usd==1]
    } # !!! I am not sure this error is still there
    
    # Expressed in USD: create dummy = 1 if currency is USD
    df <- df[, in_usd := ifelse(SIT_CURRENCY_ID == "DOL", 1, 
        ifelse(SIT_CURRENCY_ID == "ARG", 0, NA))]
    df$SIT_CURRENCY_ID <- NULL
    
    # Drop observations where currency is USD but actual price is different from price in USD
    df <- df[!(in_usd == 1 & 
                             (ITE_CURRENT_PRICE != ITE_BASE_CURRENT_PRICE)) | 
                           MesListing %in% c("2022-05-01","2022-06-01","2022-07-01")]
    # (Exception for months with errors)
    
    # Drop observations where currency is PESOS but actual price is equal to price in USD
    df <- df[!(in_usd == 0 & 
                             (ITE_CURRENT_PRICE == ITE_BASE_CURRENT_PRICE))]
    
    # Create: Price in USD
    # Load exchange rate db
    path_ <- file.path(getwd(), "Data", "Economy", "TC Blue", "dolar_series.csv") 
    er <- read.csv(path_, row.names = NULL)
    
    # Calculate average ER by month
    er$fecha <- as.IDate(er$fecha,"%Y-%m-%d")
    er$mes <- as.IDate(format(er$fecha,"%Y-%m-01"))
    er <- er %>%
        group_by(mes) %>%
        summarise(dollar_price = mean(compra))
    # Join ER values to df
    df <- merge(df, er, by.x = "MesListing", by.y="mes", all.x = TRUE)
    
    # Create variable PRICE IN USD
    df$price_usd <- ifelse(df$in_usd==1, df$ITE_BASE_CURRENT_PRICE,
                           df$ITE_CURRENT_PRICE/df$dollar_price)
    rm(er)
    df$ITE_BASE_CURRENT_PRICE <- NULL
    
    # PRICE IN REAL PESOS (Dec 2022=100)
    path_ <- file.path(getwd(), "Data", "Economy", "Inflation", "infla_nacional_INDEC.csv")
    infla <- read.csv(path_, row.names = NULL)
    infla$index2 <- infla$index/100
    infla$month <- as.Date(infla$month)
    infla <- infla[,c('month','index2')]
    
    # Use Dec 2022 as base month
    infla$index2 <- infla$index2 / infla$index2[infla$month=="2022-12-01"]
    
    df <- merge(df, infla, by.x = "MesListing", by.y="month", all.x = TRUE)
    rm(infla)
    df$price_pesos <- df$ITE_CURRENT_PRICE
    df$price_pesos[df$in_usd==1] <- df$ITE_CURRENT_PRICE[df$in_usd==1] * df$dollar_price[df$in_usd==1]
    df$price_pesos_real <- df$price_pesos / df$index2
    
    df$index2 <- NULL
    df$dollar_price <- NULL
    
    # REAL PRICE PER M2
    df$pm2_covered_pesos_real <- df$price_pesos_real/df$SConstrM2
    df$pm2_total_pesos_real <- df$price_pesos_real/df$STotalM2
    
    # PRICE PER M2 (USD)
    df$pm2_covered_usd <- df$price_usd/df$SConstrM2
    df$pm2_total_usd <- df$price_usd/df$STotalM2
    
    # Those for SALE have to be in USD
    df <- df[OPERACION=="VENTA" & in_usd==1 | OPERACION!="VENTA"]
    
    # Clean: PRICE IN USD / REAL PESOS
    # Dollar blue in Dic 2022: 323.85
    
    # Max absolute value - Sale
    df <- df[OPERACION=="VENTA" & price_usd < 25000000 |
                 OPERACION!="VENTA"]
    # df <- df[OPERACION=="VENTA" & price_pesos_real < 8096250000 |
    #              OPERACION!="VENTA"]
    
    # Max absolute value - Rent
    # df <- df[OPERACION=="ALQUILER" & price_usd < 25000 |
    #              OPERACION!="ALQUILER"]
    df <- df[OPERACION=="ALQUILER" & price_pesos_real < 8000000 |
                 OPERACION!="ALQUILER"] # 24703 USD
    # df <- df[OPERACION=="ALQUILER TEMPORAL" & price_usd < 25000 |
    #              OPERACION!="ALQUILER TEMPORAL"]
    df <- df[OPERACION=="ALQUILER TEMPORAL" & price_pesos_real < 8000000 |
                 OPERACION!="ALQUILER TEMPORAL"] # 24703 USD
    
    # Min absolute value - Sale
    df <- df[OPERACION=="VENTA" & price_usd > 10000 |
                 OPERACION!="VENTA"] # Absolute min
    # df <- df[OPERACION=="VENTA" & price_pesos_real > 3238500 |
    #              OPERACION!="VENTA"] # Absolute min

    # Min absolute value - Rent
    # df <- df[OPERACION=="ALQUILER" & price_usd > 25 |
    #              OPERACION!="ALQUILER"] # Absolute min
    df <- df[OPERACION=="ALQUILER" & price_pesos_real > 8000 |
                 OPERACION!="ALQUILER"] # Absolute min. 24.7 USD
    # df <- df[OPERACION=="ALQUILER TEMPORAL" & price_usd > 25 |
    #              OPERACION!="ALQUILER TEMPORAL"] # Absolute min
    df <- df[OPERACION=="ALQUILER TEMPORAL" & price_pesos_real > 8000 |
                 OPERACION!="ALQUILER TEMPORAL"] # Absolute min. 24.7 USD

    # Clean: PRICE PER COVERED M2 IN USD
    # Min absolute value - Sale
    df <- df[OPERACION=="VENTA" & pm2_covered_usd > 100 |
                 OPERACION!="VENTA"]
    # df <- df[OPERACION=="VENTA" & pm2_covered_pesos_real > 32385 |
    #              OPERACION!="VENTA"]
    
    # Max absolute value - Rent
    # df <- df[OPERACION=="ALQUILER" & pm2_covered_usd < 60 |
    #              OPERACION!="ALQUILER"]
    df <- df[OPERACION=="ALQUILER" & pm2_covered_pesos_real < 20000 |
                 OPERACION!="ALQUILER"] # 61.8 USD
    # df <- df[OPERACION=="ALQUILER TEMPORAL" & pm2_covered_usd < 60 |
    #              OPERACION!="ALQUILER TEMPORAL"]
    df <- df[OPERACION=="ALQUILER TEMPORAL" & pm2_covered_pesos_real < 20000 |
                 OPERACION!="ALQUILER TEMPORAL"] # 61.8 USD
    
    # Min absolute value - Rent
    # df <- df[OPERACION=="ALQUILER" & pm2_covered_usd > 1 |
    #              OPERACION!="ALQUILER"]
    df <- df[OPERACION=="ALQUILER" & pm2_covered_pesos_real > 350 |
                 OPERACION!="ALQUILER"] # almost 1 USD
    # df <- df[OPERACION=="ALQUILER TEMPORAL" & pm2_covered_usd > 1 |
    #              OPERACION!="ALQUILER TEMPORAL"]
    df <- df[OPERACION=="ALQUILER TEMPORAL" & pm2_covered_pesos_real > 350 |
                 OPERACION!="ALQUILER TEMPORAL"] # almost 1 USD
    


    # # Inconsistent coordinates # !!! NO. This is data leakage
    # # Calculate standard deviation of lat/lon for unique values by ID
    # df <- df[, c("sd_lat", "sd_lon") := 
    #                        .(sd(LATITUDE[!duplicated(LATITUDE)]), 
    #                          sd(LONGITUDE[!duplicated(LONGITUDE)])), 
    #                    by = ITE_ITEM_ID]
    # df <- df[, sd_lat := ifelse(is.na(sd_lat), 0, sd_lat)]
    # df <- df[, sd_lon := ifelse(is.na(sd_lon), 0, sd_lon)]
    # 
    # 
    # # Remove observations where the SD is greater than X
    # df <- df[sd_lat<0.0001 & sd_lon<0.0001]
    # df$sd_lat <- NULL
    # df$sd_lon <- NULL
    # 
    # # FIX lat/lon (impute the last value for all the rows with the same ID)
    # df <- df[, c("LATITUDE", "LONGITUDE") := 
    #                        .(last(LATITUDE), last(LONGITUDE)), 
    #                    by = ITE_ITEM_ID]
    
    # City: remove tildes and transform to uppercase
    df$ITE_ADD_CITY_NAME <- stringi::stri_trans_general(
        df$ITE_ADD_CITY_NAME,"Latin-ASCII")
    df$ITE_ADD_CITY_NAME <- toupper(df$ITE_ADD_CITY_NAME)
    
    # Neighbourhood: remove tildes and transform to uppercase
    df$ITE_ADD_NEIGHBORHOOD_NAME <- stringi::stri_trans_general(df$ITE_ADD_NEIGHBORHOOD_NAME,"Latin-ASCII")
    df$ITE_ADD_NEIGHBORHOOD_NAME <- toupper(df$ITE_ADD_NEIGHBORHOOD_NAME)
    
    # # Check if all observations of each listing are in the same neighborhood
    # # !!! NO. This is data leakage
    # df_grouped <- df[, .(uniform_neighb = length(unique(ITE_ADD_NEIGHBORHOOD_NAME)) == 1), 
    #                       by = ITE_ITEM_ID]
    # 
    # # Filter and remove non-uniform observations
    # uniform_group_ids <- df_grouped[uniform_neighb == TRUE, ITE_ITEM_ID]
    # df <- df[ITE_ITEM_ID %in% uniform_group_ids]
    # rm(uniform_group_ids, df_grouped)
    # gc()
    
    
    # DROP DUPLICATED OBSERVATIONS

    # Drop HOUESS with identical COORDINATES, TOTAL M2 and BUILT M2
    #   in the same month
    df <- df[, dup_id := 1:.N, 
                       by = .(OPERACION, MesListing, LONGITUDE, LATITUDE, house, STotalM2, 
                              SConstrM2)]
    df <- df[, is_duplicated := dup_id > 1 & house==1]
    df <- df[is_duplicated==F]
    
    # Drop FLATS with identical COORDINATESS, PRICE, TOTAL M2 and BUILT M2, BEDROOMS,
    #   BATHROOMS, ROOMS in the same month
    df <- df[, dup_id := 1:.N, 
                       by = .(OPERACION, MesListing, LONGITUDE, LATITUDE, ITE_CURRENT_PRICE, 
                              house, STotalM2, SConstrM2, Dormitorios, Banos, 
                              Ambientes)]
    df <- df[, is_duplicated := dup_id > 1 & house==0 ]
    df <- df[is_duplicated==F]
    df$ITE_CURRENT_PRICE <- NULL
    
    # Remove the dup_id and is_duplicated columns
    df <- df[, c("dup_id", "is_duplicated") := NULL]
    
    gc()
    
    # FIX VARIABLES
    
    # For all binary variables, convert to integer boolean
    print("Check missing values:")
    print(sapply(df, function(x) sum(is.na(x))))
    
    cols <- c("SalonFiestas", "AreaJuegosInfantiles", "BusinessCenter", 
              "SalonDeUsosMul", "Amoblado", "Estacionamiento", "Pileta",
              "Seguridad", "Gimnasio", "PistaJogging", "Calefaccion", "AireAC",
              "AreaParrillas", "CanchaTennis", "AreaCine")
    for(col in cols){
        
        if(verbose){
            # Report the % of missing values for that variable
            print(glue::glue("Percentaje of missing values for variable {col}: {round(sum(is.na(df[[col]]))/nrow(df)*100)}"))
        }
        
        # remove tildes
        df[[col]] <- stringi::stri_trans_general(df[[col]],"Latin-ASCII")
        # upper case
        df[[col]] <- toupper(df[[col]])
        # remove blank spaces
        df[, (col) := gsub(" ", "", get(col))]
        # translate characters
        df[get(col) %in% c("SI", "S?", "S??"), (col) := "1"]
        df[get(col) == "NO", (col) := "0"]
        
        if(!all(unique(df[[col]]) %in% c("0", "1", NA))){
            stop(glue::glue("Error: not all unique values for variable {col} were contemplated."))
        }
        
        # Consider NAs as 0s
        df[is.na(get(col)), (col) := "0"]
        
        # Convert to numeric
        df[[col]] <- as.numeric(df[[col]])
    }
    
    # Cocheras
    df$Cocheras <- as.numeric(df$Cocheras)
    df$Cocheras <- ifelse(df$Cocheras==0, 0, 1)
    
    
    ### CREATE NEW VARIABLES
    
    # Parking
    df <- df[, Parking := as.integer(Estacionamiento == 1 |
                                            Cocheras == 1)]
    df <- df[, c("Estacionamiento", "Cocheras"):=NULL]
    
    # Common area
    df <- df[, CommonArea := as.integer(SalonDeUsosMul == 1 |
        AreaJuegosInfantiles == 1 | BusinessCenter == 1 | SalonFiestas == 1 |
            AreaParrillas == 1 | AreaCine == 1)]
    df <- df[, c("SalonDeUsosMul", "AreaJuegosInfantiles", "BusinessCenter",
                 "SalonFiestas", "AreaParrillas", "AreaCine"):=NULL]
    
    # Fitness area
    df <- df[, FitnessArea := as.integer(Gimnasio == 1 | 
        PistaJogging == 1 | CanchaTennis == 1)]
    df <- df[, c("Gimnasio", "PistaJogging", "CanchaTennis"):=NULL]
    
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
        directories <- c("Plots", "DataCleaning", "CleanByValues")
        for (dir_name in directories) {
            path_ <- file.path(path_, dir_name)
            if (!dir.exists(path_)) {
                dir.create(path_)
            }
        }
        ggsave(file.path(path_, glue::glue("n_listings_{year}.png")), 
               plot, width = 8, height = 6, dpi = 300)
    }
    
    # REPORT
    if(verbose){
        print(glue::glue("{n_rows_start} observations before any cleaning. {nrow(df)} left after cleaning."))
        cat("\n")
        
        print("Check missing values:")
        print(sapply(df, function(x) sum(is.na(x))))
    }
    
    print(glue::glue("Finished: Clean Incorrect Data for year {year}"))
    cat('\n')
    return(df)
    
}