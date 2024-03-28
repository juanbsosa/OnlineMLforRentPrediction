#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#              REMOVE OUTLIERS                #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)


# Function -----------------------------------------------------------------

remove_outliers <- function(
        df,
        verbose=T,
        browse=F
){
    
    # Browse
    if(browse){browser()}
    

# Outliers by price_per_covered_sqm PER M2 ------------------------------------------------
    
    # Initialize a list to store the results for each group
    result_list <- list(neigh = data.table(), 
                        commune = data.table(), 
                        commune_flatsandhouses = data.table())
    
    # Define a list of grouping variables for each operation
    # By neighbourhood and property type
    groupings <- list(neigh = list(vars = c("neighbourhood", "house"), 
                                   result_name = c("percentile_01_neigh",
                                                   "percentile_99_neigh")),
                      # By commune and property type
                      commune = list(vars = c("commune", "house"), 
                                    result_name = c("percentile_01_comm",
                                                     "percentile_99_comm")),
                      # By commune only
                      commune_flatsandhouses = list(vars = c("commune"), 
                                    result_name = c("percentile_01_comm2",
                                                    "percentile_99_comm2")))
    
    unique_months <- unique(df$listing_month)[order(unique(df$listing_month))]
    
    for (month in unique_months) {
    # # Start from 2018
    # for (month in unique_months[format(unique_months, "%Y") > 2017]) {
        
        month <- as.Date(month, format="%Y-%m-%d")
        month_minus_11 <- month - months(11)
        last_12_months <- unique_months[unique_months >= month_minus_11 & 
                                            unique_months <= month]
        filtered_data <- df[listing_month %in% last_12_months]
        min_listings_valid <- 10
        
        # Loop over each grouping
        for (group in names(groupings)) {
            percentiles <- filtered_data[,
                .(
                    percentile01 = quantile(price_realpesos_per_covered_sqm, 0.01, 
                                          na.rm = TRUE),
                    percentile99 = quantile(price_realpesos_per_covered_sqm, 0.99, 
                                          na.rm = TRUE), 
                    n = .N),
                                           by = eval(groupings[[group]]$vars)]
            
            percentiles <- percentiles[n >= min_listings_valid]
            percentiles[, n := NULL]
            
            percentiles[, listing_month := as.IDate(month, format="%Y-%m-%d")]
            
            setnames(percentiles, c("percentile01", "percentile99"),
                     groupings[[group]]$result_name)

            result_list[[group]] <- rbindlist(list(result_list[[group]], percentiles))
        }
    }
    
    # Merge back to the original dataset
    df <- merge(df, result_list$neigh, by=c("listing_month", "neighbourhood", "house"), all.x = TRUE, all.y = FALSE)
    df <- merge(df, result_list$commune, by=c("listing_month", "commune", "house"), all.x = TRUE, all.y = FALSE)
    df <- merge(df, result_list$commune_flatsandhouses, by=c("listing_month", "commune"), all.x = TRUE, all.y = FALSE)
    
    # Check that there is at least one value for all observations
    if(any(is.na(df$percentile_01_comm2[format(df$listing_month, "%Y") > 2017]))){stop("There are observations without a 1th percentile (excluding year 2017).")}
    if(any(is.na(df$percentile_99_comm2[format(df$listing_month, "%Y") > 2017]))){stop("There are observations without a 99th percentile (excluding year 2017).")}
    
    rm(filtered_data, last_12_months, month_minus_11, groupings, percentiles, 
       result_list, group, month, unique_months, min_listings_valid)
    
    if(browse){browser()}
    
    # Remove observations above the corresponding percentile
    if(verbose){
        print("Removing observations according to percentiles 1 and 99 of price per covered m2.")
        cat("\n")
    }
    # 1)
    df <- df[is.na(percentile_01_neigh) | 
                 price_realpesos_per_covered_sqm >= percentile_01_neigh]
    df <- df[is.na(percentile_99_neigh) | 
                 price_realpesos_per_covered_sqm <= percentile_99_neigh]
    # 2)
    df <- df[
        # either the first filter was applied
        !is.na(percentile_01_neigh) | 
            # or it was not applied, but we cannot apply the second filter either
            (is.na(percentile_01_neigh) & is.na(percentile_01_comm)) | 
            # or it was not applied, so apply the second filter
            (is.na(percentile_01_neigh) & 
                 price_realpesos_per_covered_sqm >= percentile_01_comm)]
    df <- df[
        # either the first filter was applied
        !is.na(percentile_99_neigh) | 
            # or it was not applied, but we cannot apply the second filter either
            (is.na(percentile_99_neigh) & is.na(percentile_99_comm)) | 
            # or it was not applied, so apply the second filter
            (is.na(percentile_99_neigh) & price_realpesos_per_covered_sqm <= percentile_99_comm)]
    # 3)
    df <- df[
        # either the first filter was applied
        !is.na(percentile_01_neigh) | 
            # or the second filter was applied
            !is.na(percentile_01_comm) |
            # or none were applied, so apply the third filter
            # or it was not applied, so apply the second filter
            (is.na(percentile_01_neigh) & is.na(percentile_01_comm) & 
                 price_realpesos_per_covered_sqm >= percentile_01_comm2)] 
    df <- df[
        # either the first filter was applied
        !is.na(percentile_99_neigh) | 
            # or the second filter was applied
            !is.na(percentile_99_comm) |
            # or none were applied, so apply the third filter
            # or it was not applied, so apply the second filter
            (is.na(percentile_99_neigh) & is.na(percentile_99_comm) & 
                 price_realpesos_per_covered_sqm <= percentile_99_comm2)]
    
    # Remove variables
    df <- df %>% select(-c(percentile_01_neigh, percentile_99_neigh, 
                           percentile_01_comm, percentile_99_comm,
                           percentile_01_comm2, percentile_99_comm2))
    
    gc()
    return(df)
    
}