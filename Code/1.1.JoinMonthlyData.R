#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#   JOIN MONTHLY DATA INTO YEARLY DATA              #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(ggplot2)

# TO DOs:
# - Plots have to be made by OPERACION


# Function -----------------------------------------------------------------

join_monthly_dbs <- function(
        year,
        path_data_in="Data/Meli/Raw/AMBA",
        starting_month=1,
        ending_month=12,
        max_rows=Inf,
        verbose=T,
        plot_checks=T,
        browse=F
){
    
    if(browse){browser()}
    
    # For Dec 2022, data is in Jan 2023. I call it 13/2023
    exception <- FALSE
    if(year==2022 & ending_month==12){ending_month <- 13}
    
    for(month in starting_month:ending_month){
        
        if(browse){browser()}
        
        # Skip month of August because the complete data for this
        # month is in September
        if(year==2022 & month==8){next}
        
        # Accomodate Jan 2023
        if(year==2022 & month==13){
            year <- 2023
            month <- 1
            exception <- TRUE
        }
        
        # Find files with data for that month
        month_parsed <- sprintf("%02d", month)
        find_pattern <- glue::glue(".*{year}{month_parsed}.*.csv")
        path_ <- file.path(getwd(), path_data_in, year)
        files <- list.files(path=path_, 
                            pattern=find_pattern)
        print("File list for the month:")
        print(files)
        cat("\n")
        
        # Loop over all files for that month
        for(i in 1:length(files)){
            
            # Read the file
            print(paste("Reading the file", files[1], "..."))
            path_ <- file.path(getwd(), path_data_in, year, files[i])
            
            # Only read some columns
            cols <- c(
                # dates
                "MesListing",
                # id
                "ITE_ITEM_ID",
                # operation
                "OPERACION",
                # prices
                "ITE_BASE_CURRENT_PRICE", "ITE_CURRENT_PRICE",
                # "ITE_SITE_CURRENT_PRICE",
                # currency
                "SIT_CURRENCY_ID",
                # property type
                # "TIPOPROPIEDADNORM",
                "TIPOPROPIEDAD",
                # area
                "STotalM2", "SConstrM2",
                # amenities
                "Dormitorios", "Banos", "Ambientes", 
                "Antiguedad", 
                "Estacionamiento", "Cocheras",
                "Pileta",
                "Gimnasio", "PistaJogging", "CanchaTennis",
                "Seguridad", 
                "Amoblado", 
                "Calefaccion", 
                "AireAC",
                "SalonFiestas",  "SalonDeUsosMul", "AreaJuegosInfantiles", 
                "BusinessCenter", "AreaParrillas", "AreaCine", 
                
                # location
                "ITE_ADD_CITY_NAME", "ITE_ADD_NEIGHBORHOOD_NAME", 
                "ITE_ADD_STATE_NAME",
                "LONGITUDE", "LATITUDE",
                # Meli
                # "TIM_DAY", 
                "ITE_AUCTION_START"
                # "ITE_AUCTION_STOP",
            )
            
            # Read dataset
            df_monthly <- fread(path_, encoding = "UTF-8", 
                                nrows=max_rows, select=cols, fill=TRUE)
            
            # Remove rows without month
            df_monthly <- df_monthly[!is.na(MesListing)]
            
            # Fix variable values

            # In some dfs the dates are separated by "/" instead of "-"
            if(year %in% c(2021) & month %in% c(4,6,7,8,11)){
                
                cols <- c("MesListing", "ITE_AUCTION_START")
                for(col in cols){
                    df_monthly[[col]] <- as.IDate(df_monthly[[col]], "%d/%m/%Y")
                }
                
                
            }
            
            
            # Fix some column classes

            
            # If the class of these columns is X, convert to Y
            
            # Character to Numeric
            cols <- c("STotalM2", "SConstrM2")
            for(col in cols){
                if(class(df_monthly[[col]]) %in% c("character")){
                    df_monthly[[col]] <- as.numeric(df_monthly[[col]])
                }
            }
            
                # Integer to Numeric
            cols <- c("STotalM2", "SConstrM2", "ITE_CURRENT_PRICE", "Ambientes")
            for(col in cols){
                if(class(df_monthly[[col]]) %in% c("integer", "integer64")){
                    df_monthly[[col]] <- as.numeric(df_monthly[[col]])
                }
            }
                # Integer to Character
            cols <- c("Banos", "Dormitorios", "Antiguedad", "Estacionamiento",
                      "Cocheras",
                      "Pileta", "Gimnasio", "Seguridad", "SalonFiestas",
                      "SalonDeUsosMul", "BusinessCenter", "Amoblado", 
                      "AreaJuegosInfantiles", "Calefaccion", "AireAC", 
                      "AreaParrillas", "PistaJogging",
                      "CanchaTennis", "AreaCine")
            for(col in cols){
                if(class(df_monthly[[col]])=="integer"){
                    df_monthly[[col]] <- as.character(df_monthly[[col]])
                }
            }
            
            # Logical to character
            cols <- c("AreaJuegosInfantiles", "PistaJogging", "AreaCine",
                "Estacionamiento", "Seguridad", "SalonFiestas", "Gimnasio",
                "BusinessCenter", "CanchaTennis", "SalonDeUsosMul", "Amoblado")
            for(col in cols){
                if(class(df_monthly[[col]])=="logical"){
                    df_monthly[[col]] <- as.character(df_monthly[[col]])
                }
            }
            
            # POSIXct to IDate
            cols <- c("ITE_AUCTION_START")
            for(col in cols){
                if(class(df_monthly[[col]])[1]=="POSIXct"){
                    df_monthly[[col]] <- as.IDate(df_monthly[[col]])
                }
            }
            
            if(month==starting_month & i==1){
                
                # Print info about column classes
                if(verbose){
                    print("Variable details for the first month analysed:")
                    print(str(df_monthly))
                    cat("\n")
                }
                
                # Store column classes ( make sure there is only one string per column)
                column_classes <- sapply(df_monthly, class)
                column_classes <- sapply(column_classes, function(class_str) {
                    first_class <- strsplit(class_str, " ")[[1]]
                    first_class[1]
                })
                
            } else {
                
                # For other months, compare column classes with the starting month
                print("Checking if all column classes match with the starting month...")
                
                # Store column classes ( make sure there is only one string per column)
                column_classes_aux <- sapply(df_monthly, class)
                column_classes_aux <- sapply(column_classes_aux, 
                    function(class_str) {
                        first_class <- strsplit(class_str, " ")[[1]]
                        first_class[1]
                        })
                
                # If they dont, reporte error
                if(any(
                    unlist(column_classes_aux)!=
                    unlist(column_classes)
                )){
                    
                    print("Column classes do not match with the starting month")
                    print("The columns whose classes do not match are:")
                    cols <- colnames(df_monthly)[which(column_classes_aux!=column_classes)]
                    print(sapply(df_monthly[, ..cols], class))
                    
                    print("First 10 unique values for the columns (in the current df) whose classes do not match:")
                    print(sapply(sapply(df_monthly[, ..cols], unique), head, 10))
                    cat("\n")
                    
                    stop("Error: Column classes do not match with the starting month")
                    
                }
                cat("\n")
                
            }
            
            # Check months in database (there should be maximum 2)
            if(length(unique(df_monthly$MesListing))>2){
                stop("Error: more than two months in this file.")
            }
            print(paste("Unique months before filtering:",
                        paste(unique(df_monthly$MesListing), collapse=", ")))
            cat("\n")
            
            # Keep only observations for that month
            # Except for some months that have the complete data for the previous month
            if(year==2022 & month>=9){
                
                month_tminus1_parsed = sprintf("%02d", month-1)
                df_monthly <- df_monthly[MesListing==glue::glue("{year}-{month_tminus1_parsed}-01")]
                rm(month_tminus1_parsed)
                
            } else if(year==2023 & month==1){
                
                df_monthly <- df_monthly[MesListing=="2022-12-01"]
                
            }else{
                
                df_monthly <- df_monthly[MesListing==glue::glue("{year}-{month_parsed}-01")]
                
            }
            
            # For the first file in the month, rename the df
            if(i==1){
                df_monthly_full <- df_monthly
            } else{
                
            # For the rest of the files in the month, bind it with the first df
                df_monthly_full <- rbind(df_monthly_full, df_monthly)
                
            }
            
            rm(df_monthly)
            gc()
            
        } # end of files loop
        
        if(browse){browser()}
        
        # If this is the first month of the year, then this will be the base df
        if(month==starting_month & exception==F){
            
            df_base <- df_monthly_full

            # If not, bind monthly df to base df
        } else{
            df_base <- rbind(df_base, df_monthly_full)
        }
        
        rm(df_monthly_full)
        gc()
        
        
    } # end of month loop
    
    if(browse){browser()}
    
    if(verbose){
        print("Number of observations by month:")
        print(table(df_base$MesListing, useNA="always"))
        
    }
    
    # Accomodate Jan 2023
    if(year==2023 & month==1){
        year <- 2022
        exception <- FALSE
    }
    
    if(plot_checks){
        # Plot the evolution of the number of listings, just to check if any month is missing
        print("Creating plot of the number of listings by month...")
        cat("\n")
        plot <- ggplot(df_base[, .(Observations = .N), by = MesListing], 
                       aes(x = MesListing, y = Observations, group = 1)) +
            geom_line() +
            geom_point() +
            scale_y_continuous(limits = c(0, 1000000)) +
            labs(x = "Month", y = "Number of Observations", title = "Observations by Month")
        
        # Save plot
        path_ <- file.path(getwd(), "Output")
        directories <- c("Plots", "DataCleaning", "JoinMonthlyData")
        for (dir_name in directories) {
            path_ <- file.path(path_, dir_name)
            if (!dir.exists(path_)) {
                dir.create(path_)
            }
        }
        ggsave(file.path(path_, glue::glue("n_listings_{year}.png")), 
               plot, width = 8, height = 6, dpi = 300)
    }
    
    
    # Return df
    print(glue::glue("Finished: Join Monthly Data for year {year}"))
    cat('\n')
    return(df_base)
    
}