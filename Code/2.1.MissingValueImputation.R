#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#              IMPUTE MISSING VALUES                #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(dplyr)

# !!! I am not considering the fact that time passes. I cannot simply use all
# previous months to impute in the present, specially with the last years in
# the data set


# Function -----------------------------------------------------------------

impute_missings <- function(
        df,
        verbose=T,
        browse=F
){
    
    if(browse){browser()}
    
    # MEAN IMPUTATION FOR AGE, RECURSIVELY
    
    # Create a sequence of unique months where there are missing values for age
    unique_months <- unique(df$listing_month[is.na(df$property_age)])
    
    # Calculate mean property_age for all observations up to and including each month
    # grouped by neighbourhood and house
    mean_results <- lapply(unique_months, function(m) {
        df %>%
            filter(listing_month <= m) %>%
            group_by(neighbourhood, house) %>%
            summarise(mean_property_age = mean(property_age, na.rm = TRUE), 
                      listing_month = m,
                      # n=n()
                      )
    })
    
    # Combine the results into a single data frame
    mean_results_df <- bind_rows(mean_results)
    rm(mean_results)
    gc()
    
    # # AUX: visualize
    # aux <- mean_results_df %>% filter(house==1 & listing_month>="2020-09-01" &
    #     neighbourhood %in% c("BELGRANO", "BALVANERA", "RECOLETA", "SAAVEDRA",
    #                          "VILLA LUGANO", "VILLA SOLDATI"))
    # ggplot(aux, aes(x = listing_month, 
    #                                           y = mean_property_age, color = factor(neighbourhood))) +
    #     geom_line() +
    #     # facet_wrap(~ neighbourhood, scales = "free_y", ncol = 2) +
    #     labs(title = "Historic mean property Age, for houses",
    #          x = "Month",
    #          y = "Mean Property Age") +
    #     theme_minimal()
    # 
    # aux <- mean_results_df %>% filter(house==0 & listing_month>="2020-09-01" &
    #     neighbourhood %in% c("BELGRANO", "BALVANERA", "PUERTO MADERO", "SAAVEDRA",
    #         "VILLA LUGANO", "VILLA SOLDATI"))
    # ggplot(aux, aes(x = listing_month, 
    #                 y = mean_property_age, color = factor(neighbourhood))) +
    #     geom_line() +
    #     # facet_wrap(~ neighbourhood, scales = "free_y", ncol = 2) +
    #     labs(title = "Historic mean Property Age, for flats",
    #          x = "Month",
    #          y = "Mean Property Age") +
    #     theme_minimal()

    # Merge to df
    df <- left_join(df, mean_results_df, 
                    by=c("listing_month", "neighbourhood", "house"))
    
    # Impute
    df$property_age[is.na(df$property_age)] <- df$mean_property_age[is.na(df$property_age)]
    df$mean_property_age <- NULL
    
    if(any(is.na(df$property_age))){
        stop("Erorr: there are still missing values for property_age. Imputation was insufficient.")
    }
    
    # Check missing values
    if(any(is.na(df))){
        stop("ERROR: There are still missing values in the data frame.")
    }
    
    return(df)
    
    
}