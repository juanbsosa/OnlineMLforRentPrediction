#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#                   REMOVE OUTLIERS                 #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(dplyr)
library(openxlsx)

# TO DOs:
# The mean imputation for Age could be grouped by each month. Right now it is by year

# Function -----------------------------------------------------------------

clean_outliers <- function(
        df,
        year,
        verbose=T,
        plot_checks=T,
        browse=F
){
    
    if(browse){browser()}
    
    # Report initial number of rows
    n_rows_start <- nrow(df)
    if(verbose){
        print(glue::glue("{n_rows_start} observations before any cleaning."))
        cat("\n")
    }
    
    

    # Impute ------------------------------------------------------------------

    # AGE: mean by operation, property type, and neighbourhood
    df <- df[, mean_age := mean(Antiguedad, na.rm=T), 
             by = .(house, neighborhood)]
    df$Antiguedad[is.na(df$Antiguedad)] <- df$mean_age[is.na(df$Antiguedad)]
    # If some are still NA, go for a bigger group
    if(any(is.na(df$Antiguedad))){
        df <- df[, mean_age := mean(Antiguedad, na.rm=T), 
                 by = .(neighborhood)]
        df$Antiguedad[is.na(df$Antiguedad)] <- df$mean_age[is.na(df$Antiguedad)]
        
        if(any(is.na(df$Antiguedad))){
            df <- df[, mean_age := mean(Antiguedad, na.rm=T), 
                     by = .(neighborhood)]
            df$Antiguedad[is.na(df$Antiguedad)] <- df$mean_age[is.na(df$Antiguedad)]
            
            if(any(is.na(df$Antiguedad))){
                df <- df[, mean_age := mean(Antiguedad, na.rm=T), 
                         by = .(house)]
                df$Antiguedad[is.na(df$Antiguedad)] <- df$mean_age[is.na(df$Antiguedad)]
            }
            
        }
        

        
    }
    df$mean_age <- NULL

    # Calculate thresholds ----------------------------------------------------
    
    # Calculate quantiles by property type and neighbourhood
    df <- df[, q99_m2_uncovered := quantile(uncovered_m2, probs = 0.99), 
             by = .(house, neighborhood)]
    df <- df[, q99_m2_covered := quantile(SConstrM2, probs = 0.99), 
             by = .(house, neighborhood)]
    df <- df[, num_observations_surfq := .N, by = .(house, neighborhood)]
    # # Aux: see results
    result <- df[, .(q99_m2_uncovered = mean(q99_m2_uncovered),
                     max_m2_uncovered = max(uncovered_m2),
                     q99_m2_covered = mean(q99_m2_covered),
                     max_m2_covered = max(SConstrM2),
                     num_observations = .N),
                 by = .(house, neighborhood)]
    rm(result)
    
    # Calculate quantiles by property type and neighbourhood
    df <- df[, q99_pm2_covered_usd := quantile(pm2_covered_usd, probs = 0.99), 
             by = .(house, OPERACION, neighborhood)]
    df <- df[, q99_price_usd := quantile(price_usd, probs = 0.99),
             by = .(house, OPERACION, neighborhood)]
    df <- df[, num_observations_priceq := .N, by = .(house, neighborhood)]
    # # Aux: see results
    result <- df[, .(q99_pm2_covered_usd = mean(q99_pm2_covered_usd),
                     max_pm2_covered_usd = max(pm2_covered_usd),
                     q99_price_usd = mean(q99_price_usd),
                     max_price_usd = max(price_usd),
                     num_observations = .N),
                 by = .(house, neighborhood)]
    rm(result)
    
    
    # Remove observations -----------------------------------------------------
    
    # COVERED M2
    df <- df[(SConstrM2<q99_m2_covered & num_observations_surfq>10) |
                 num_observations_surfq<10]
    
    # UNCOVERED M2
    df <- df[(uncovered_m2<q99_m2_uncovered & num_observations_surfq>10) |
                 num_observations_surfq<10]

    # PRICE PER COVERED M2 IN USD
    df <- df[(pm2_covered_usd<q99_pm2_covered_usd & num_observations_priceq>10) |
                 num_observations_priceq<10]

    # PRICE IN USD
    df <- df[(price_usd<q99_price_usd & num_observations_priceq>10) |
                 num_observations_priceq<10]

    
    # Delete columns
    df <- df[, c("q99_m2_uncovered",
            "q99_m2_covered", "q99_pm2_covered_usd", 
            "q99_price_usd", "num_observations_priceq",
            "num_observations_surfq"):=NULL]
    
    
    if(verbose==T){
        # # Missing value report
        print(glue::glue("Year {year}: missing value report."))
        print(sapply(df, function(x) sum(is.na(x))))
        cat('\n')
    }

    # More reports
    vars_to_tab <- c("MesListing", "OPERACION", 
                     "house", "in_usd",
                     "Dormitorios", "Banos", "Ambientes",
                     "Pileta", "Seguridad", "Amoblado",
                     "Calefaccion", "AireAC", "Parking", "CommonArea",
                     "FitnessArea", "neighborhood")
    
    vars_to_stat <- c("SConstrM2", "uncovered_m2", 
                      "ListingAge", "Antiguedad", 
                      "price_usd", "pm2_covered_usd"
                      )
    
    # Create an empty list to store the descriptive statistics for each combination
    descriptive_stats_list <- list()
    
    for(operation in unique(df$OPERACION)){
            
            for(is_house in c(0,1)){
                
                # Unique values report
                if(verbose==T){
                    print(glue::glue("Year {year}: unique value report for operation {operation} and house equal to {is_house}."))
                    print(sapply(df[OPERACION==operation & house==is_house, 
                                     ..vars_to_tab], function(x) table(x, useNA="always")) )
                    cat('\n')
                }
                
                # Descriptive statistics
                
                descriptive_stats <- psych::describe(df[OPERACION==operation & house==is_house, 
                                                        ..vars_to_stat])
                descriptive_stats$vars <- rownames(descriptive_stats)
                if(verbose==T){
                    print(glue::glue("Year {year}: descriptive statistics for operation {operation} and house equal to {is_house}."))
                    print(as.data.frame(descriptive_stats))
                    cat('\n')
                }
                
                # Store the descriptive statistics in the list
                key <- glue::glue("{operation}_House_{is_house}")
                descriptive_stats_list[[key]] <- descriptive_stats
                
                # Plot checks
                if(plot_checks){
                    # Plot the evolution of the number of listings, just to check if any month is missing
                    if(verbose==T){
                        print("Creating plot of the number of listings by month...")
                        cat("\n")
                    }
                    plot <- ggplot(df[OPERACION==operation & house==is_house, .(Observations = .N), by = MesListing], 
                                   aes(x = MesListing, y = Observations, group = 1)) +
                        geom_line() +
                        geom_point() +
                        scale_y_continuous(limits = c(0, 60000)) +
                        labs(x = "Month", y = "Number of Observations", title = "Observations by Month")
                    
                    # Save plot
                    path_ <- file.path(getwd(), "Output")
                    directories <- c("Plots", "DataCleaning", "CleanOutliers", year)
                    for (dir_name in directories) {
                        path_ <- file.path(path_, dir_name)
                        if (!dir.exists(path_)) {
                            dir.create(path_)
                        }
                    }
                    ggsave(file.path(path_, glue::glue("n_listings_{year}_{operation}_house{is_house}.png")), 
                           plot, width = 8, height = 6, dpi = 300)
                }
                
            }
            
            # Create an Excel workbook
            wb <- createWorkbook()
            
            # Add a sheet for each set of descriptive statistics
            for (key in names(descriptive_stats_list)) {
                addWorksheet(wb, sheetName = key)
                writeData(wb, sheet = key, x = descriptive_stats_list[[key]])
            }
            
            # Save the Excel workbook
            path_ <- file.path(getwd(), "Output")
            directories <- c("Tables", "CleanOutliers")
            for (dir_name in directories) {
                path_ <- file.path(path_, dir_name)
                if (!dir.exists(path_)) {
                    dir.create(path_)
                }
            }
            saveWorkbook(wb, file = file.path(path_, "descriptive_statistics.xlsx"),
                         overwrite = T)

        }
        
    # Report
    if(verbose==T){
        print(glue::glue("{n_rows_start} observations before any cleaning. {nrow(df)} left after cleaning."))
        cat("\n")
        cat("\n")
    }
    
    print(glue::glue("Finished: Clean Outliers for year {year}"))
    cat('\n')
    return(df)
        
}
