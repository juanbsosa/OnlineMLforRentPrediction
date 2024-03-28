#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#              EXPLORATORY PLOTS                 #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(glue)
library(ggplot2)

# TO-DOs
# Add option to assign tables

# Function -----------------------------------------------------------------

create_plots <- function(
        df,
        years,
        verbose=T,
        show_plot=T,
        browse=F
){
    
    if(browse){browser()}
    
    # Plot the evolution of the number of listings
    if(verbose==T){
        print("Creating plot of the number of listings by month...")
        cat("\n")
    }
    
    # ALL data
    df_aux <- df[, .(Observations = .N), by = listing_month]
    
        # Add horizontal line for mean
    mean_obs <- round(mean(df_aux$Observations, na.rm = TRUE))
    
    plot_ <- ggplot(df_aux, 
                   aes(x = listing_month, y = Observations, group = 1)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = mean_obs, color = "red", linetype="dashed") + 
        # Add horizontal line for the mean
        annotate("text", x = unique(df_aux$listing_month)[10], y = mean_obs + 500, 
                 # Adjust y for position above line
                 label = glue("Mean = {round(mean_obs, 2)}"), color = "red") + 
        # Add annotation
        # scale_y_continuous(limits = c(0, 60000)) +
        # labs(title = "Number of Observations by Month")+
        labs(x = "Month", y = "Number of Observations")+
        scale_y_continuous(limits = c(0,NA))+
        scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y",
                     limits = as.Date(c(glue("{min(unique(df$year))}-01-01"), 
                                        "{max(unique(df$year))}-12-31")),
                     expand = c(0, 0))+
        theme(title = element_text(size=20),
              axis.title = element_text(size=16),
            axis.text.x = element_text(angle = 45, hjust = 1, size=14),
            axis.text.y = element_text(size=14))
    
    if(show_plot){print(plot_)}
    
    # Save plot
    path_ <- file.path(getwd(), "Output")
    directories <- c("Plots", "ExploratoryAnalysis")
    for (dir_name in directories) {
        path_ <- file.path(path_, dir_name)
        if (!dir.exists(path_)) {
            dir.create(path_)
        }
    }
    file_name <- paste0("n_listings_",  tolower(operation), "_", years[1], "_", 
                        years[length(years)], ".png")
    ggsave(file.path(path_,file_name), plot_, width = 12, height = 6, dpi = 300)
    
    
    # Plot the evolution of the price per square meter
    if(verbose==T){
        print("Creating plot of the price per square meter by month...")
        cat("\n")
    }
    
    # ALL data
    df_aux <- df[, .(median_price = median(price_realpesos_per_covered_sqm)), by = listing_month]
    
    # Add horizontal line for mean
    median_price_overall <- round(median(df_aux$median_price, na.rm = TRUE))
    
    plot_ <- ggplot(df_aux, 
                    aes(x = listing_month, y = median_price, group = 1)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = median_price_overall, color = "red", linetype="dashed") + 
        # Add horizontal line for the mean
        annotate("text", x = unique(df_aux$listing_month)[10], y = median_price_overall + 500, 
                 # Adjust y for position above line
                 label = glue("Median = {round(median_price_overall, 2)}"), color = "red") + 
        # Add annotation
        labs(title = "Median price by month")+
        labs(x = "Month", y = "Argentine pesos of December 2022")+
        scale_y_continuous(limits = c(0,NA))+
        scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y",
                     limits = as.Date(c(glue("{min(unique(df$year))}-01-01"), 
                                        "{max(unique(df$year))}-12-31")),
                     expand = c(0, 0))+
        theme(title = element_text(size=20),
              axis.title = element_text(size=16),
              axis.text.x = element_text(angle = 45, hjust = 1, size=14),
              axis.text.y = element_text(size=14))
    
    if(show_plot){print(plot_)}
    
    # Save plot
    path_ <- file.path(getwd(), "Output")
    directories <- c("Plots", "ExploratoryAnalysis")
    for (dir_name in directories) {
        path_ <- file.path(path_, dir_name)
        if (!dir.exists(path_)) {
            dir.create(path_)
        }
    }
    file_name <- paste0("median_price_",  tolower(operation), "_", years[1], "_", 
                        years[length(years)], ".png")
    ggsave(file.path(path_,file_name), plot_, width = 12, height = 6, dpi = 300)
    
    
    # Plot the evolution of the price per square meter (first month = 100)
    if(verbose==T){
        print("Creating plot of the price per square meter by month (first month = 100)...")
        cat("\n")
    }

    # ALL data
    df_aux <- df[, .(median_price = mean(price_realpesos_per_covered_sqm)), by = listing_month]

    # Divide by first month and multiply by 100 to get index
    df_aux$median_price <- df_aux$median_price / df_aux$median_price[1] * 100
    
    # # Add horizontal line for mean
    # median_price_overall <- round(median(df_aux$median_price, na.rm = TRUE))
    
    plot_ <- ggplot(df_aux, 
                    aes(x = listing_month, y = median_price, group = 1)) +
        geom_line() +
        geom_point() +
        # geom_hline(yintercept = median_price_overall, color = "red", linetype="dashed") + 
        # # Add horizontal line for the mean
        # annotate("text", x = unique(df_aux$listing_month)[10], 
        #          y = median_price_overall*1.15, 
        #          # Adjust y for position above line
        #          label = glue("Mean value = {round(median_price_overall, 1)}"), 
        #          color = "red",
        #          size=6) + 
        # Add annotation
        # labs(title = "Index of median price by month")+
        labs(x = "Month", y = "Median price index (Jan 2018 = 100)")+
        scale_y_continuous(limits = c(round(min(df_aux$median_price)*0.9, -1), 
                                      round(max(df_aux$median_price)*1.1, -1)))+
        scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y",
                     limits = as.Date(c(glue("{min(unique(df$year))}-01-01"), 
                                        "{max(unique(df$year))}-12-31")),
                     expand = c(0, 0))+
        theme(title = element_text(size=20),
              axis.title = element_text(size=16),
              axis.text.x = element_text(angle = 45, hjust = 1, size=14),
              axis.text.y = element_text(size=14))
    
    if(show_plot){print(plot_)}
    
    # Save plot
    path_ <- file.path(getwd(), "Output")
    directories <- c("Plots", "ExploratoryAnalysis")
    for (dir_name in directories) {
        path_ <- file.path(path_, dir_name)
        if (!dir.exists(path_)) {
            dir.create(path_)
        }
    }
    file_name <- paste0("median_price_index_",  tolower(operation), "_", years[1], "_", 
                        years[length(years)], ".png")
    ggsave(file.path(path_,file_name), plot_, width = 12, height = 6, dpi = 300)
    
}