#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#   JOIN MONTHLY DATA INTO YEARLY DATA              #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)


# Function -----------------------------------------------------------------

join_monthly_dbs <- function(
        years=2022:2022,
        path_data_in="Data/Meli/Raw/AMBA",
        # operations = c('Venta'),
        # locations = c("AMBA"),
        starting_month=7,
        ending_month=12,
        max_rows=Inf,
        assign=F,
        save=F,
        return=T,
        browser=F
){
    
    for(year in years){
        
        for(month in starting_month:ending_month){
            
            month_parsed <- sprintf("%02d", month)
            
            # Find files with data from that month
            find_pattern <- glue::glue(".*{year}{month_parsed}.*.csv")
            path_ <- file.path(getwd(), path_data_in, year)
            files <- list.files(path=path_, 
                                pattern=find_pattern)
            print("File list for the month:")
            print(files)
            cat("\n")
            
            for(i in 1:length(files)){
                
                browser()
                
                # Read the file
                print(paste("Reading the file", files[1], "..."))
                path_ <- file.path(getwd(), path_data_in, year, files[i])
                
                # Only some columns
                cols <- c(
                    # dates
                    "MesListing",
                    # id
                    "ITE_ITEM_ID",
                    # operation
                    "OPERACION",
                    # prices
                    "ITE_BASE_CURRENT_PRICE", "ITE_CURRENT_PRICE","ITE_SITE_CURRENT_PRICE",
                    "SIT_CURRENCY_ID",
                    # property type
                    "TIPOPROPIEDADNORM","TIPOPROPIEDAD",
                    # area
                    "STotalM2", "SConstrM2",
                    # amenities
                    "Dormitorios", "Banos", "Ambientes", "Antiguedad", 
                    "Estacionamiento", "Pileta",
                    "Gimnasio", "Laundry",
                    "Seguridad", "SalonFiestas",  "SalonDeUsosMul",
                    # location
                    "ITE_ADD_CITY_NAME", "ITE_ADD_NEIGHBORHOOD_NAME", "ITE_ADD_STATE_NAME"
                    # Meli
                    # "TIM_DAY", "ITE_AUCTION_START", "ITE_AUCTION_STOP",
                )
                
                df_monthly <- fread(path_, encoding = "UTF-8", 
                                    nrows=max_rows, select=cols, fill=TRUE)
                
                # if(i==1){
                #     df_monthly <- fread(path_, encoding = "UTF-8", 
                #                         nrows=max_rows, select=cols, fill=TRUE)
                # } else{
                #     df_monthly_aux <- fread(path_, encoding = "UTF-8", 
                #                             nrows=max_rows, select=cols, fill=TRUE)
                # }
                
                
                # Print info about column classes
                if(month==starting_month){

                    print("Database details:")
                    print(str(df_monthly))
                    cat("\n")

                    # Store column classes (only one element per class)
                    column_classes <- sapply(df_monthly, class)
                    column_classes <- sapply(column_classes, function(class_str) {
                        first_class <- strsplit(class_str, " ")[[1]]
                        first_class[1]
                    })

                } else {
                # For other months, compare column classes with the starting month
                
                    print("¿Do all column classes match with the starting month?")
                    column_classes_aux <- sapply(df_monthly, class)
                    column_classes_aux <- sapply(column_classes_aux, function(class_str) {
                        first_class <- strsplit(class_str, " ")[[1]]
                        first_class[1]
                    })
                    
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
                        
                    } else{
                        
                    }
                    # print(
                    #     !(
                    #         any(
                    #             unlist(column_classes_aux)!=
                    #                 unlist(column_classes)
                    #         )
                    #     )
                    # )
                    cat("\n")

                    if(any(unlist(sapply(df_monthly, class))!=unlist(column_classes))){

                        print("The columns whose classes do not match are:")
                        cols <- colnames(df_monthly)[which(sapply(df_monthly, class)!=column_classes)]
                        print(sapply(df_monthly[,cols], class))

                        print("First 10 unique values for the columns (in the current df) whose classes do not match:")
                        print(sapply(sapply(df_monthly[,cols], unique), head, 10))
                        cat("\n")

                    }

                }

                # Check months in database (there should be maximum 2)
                print(paste("Unique months before filtering:",
                            paste(unique(df_monthly$MesListing), collapse=", ")))
                cat("\n")
                
                # Filter by operation
                df_monthly <- df_monthly[OPERACION == "Venta"]
                
                # Filter by location
                df_monthly <- df_monthly[ITE_ADD_CITY_NAME == "Capital Federal" &
                                             ITE_ADD_STATE_NAME == "Capital Federal"]
                
                # Rename first df
                if(i==1){
                    df_monthly_full <- df_monthly
                } else{
                    
                # Bind other dfs
                    df_monthly_full <- rbind(df_monthly_full, df_monthly)
                    
                }
                
                rm(df_monthly)
                gc()
                
            } # end of files loop
            
            browser()
            
            
            # If this is the first month of the year, then this will be the base df
            if(month==starting_month){
                df_base <- df_monthly_full
                
            # If not, append monthly df to base df
            } else{
                df_base <- rbind(df_base, df_monthly_full)
            }
            
            rm(df_monthly_full)
            gc()

            
        }
        
        
    } # end of years loop
    
    
}