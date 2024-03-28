# Histogram of listing age by month

tab <- df %>% group_by(round(listing_age/31)) %>% summarise(n=n())
tab$pct <- tab$n/nrow(df)*100


# Missings of age vy month

plot_ <- ggplot(df_aux, 
                aes(x = listing_month, y = Observations, group = 1)) +
    geom_line() +
    geom_point() +
    # scale_y_continuous(limits = c(0, 60000)) +
    labs(title = "Number of Observations by Month")+
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

plot_+ 
    geom_line(data=df_aux2, 
              aes(x = listing_month, y = Observations, group = 1)) +
    geom_point() +
    # scale_y_continuous(limits = c(0, 60000)) +
    labs(title = "Number of Observations by Month")+
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

# Barrios del 1% mas caro
df <- dfALQ
library(ggplot2)
a <- df %>% filter(price>quantile(df$price, probs=0.99)) %>% group_by(neighbourhood) %>% 
    summarise(total=n()) %>% mutate(percentage=round(total/sum(total),1))

ggplot(a, aes(x = forcats::fct_reorder(neighbourhood, -percentage), y = percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "% by Neighbourhood of 1% more expensive properties",
         x = "Neighbourhood",
         y = "%") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

# Precio por m2 promedio del 1% mas caro por mes
df <- dfALQ
library(ggplot2)
library(dplyr)
a <- df %>% group_by(listing_month) %>% filter(price>quantile(df$price, probs=0.99)) %>%
    summarise(mean_price=mean(price)) %>% mutate(mean_price=mean_price/mean_price[1]*100)

ggplot(a, aes(x = listing_month, y = mean_price)) +
    geom_line() +
    labs(title = "Mean ",
         x = "Neighbourhood",
         y = "%") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

# Distribucion de missings de Age en el tiempo
df <- dfALQ
library(ggplot2)
library(dplyr)
a <- df %>% filter(is.na(property_age)) %>% group_by(listing_month)  %>%
    summarise(total=n()) %>% mutate(percentage=round(total/sum(total),1))

a <- df %>% group_by(listing_month)  %>%
    summarise(
        missing_property_age=sum(is.na(property_age)),
        monthly_n=n()
        ) %>% mutate(
        pct_missing_property_age=round(missing_property_age/monthly_n,3)
        )
ggplot(a, aes(x = listing_month, y = pct_missing_property_age)) +
    geom_line() +
    scale_y_continuous(limits=c(0,1))+
    labs(title = "% of missings in Property Age for each month",
         x = "t",
         y = "%") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
sum(is.na(df$property_age))/nrow(df)



# Cantidad de observaciones por barrio y mes ------------------------------


library(ggplot2)
library(dplyr)

# Your data processing steps
a <- df %>%
    group_by(neighbourhood, listing_month) %>%
    summarise(total=n()) %>% mutate(mean_n = mean(total)) %>% 
    ungroup()

# Determine the number of plots per row and per column
plots_per_row <- 3
plots_per_col <- 3

# Create a list of plots for each group of neighbourhoods
plot_list <- a %>%
    group_by(neighbourhood) %>%
    summarise(mean_value = mean(mean_value, na.rm = TRUE)) %>%
    arrange(desc(mean_value)) %>%
    group_by(group = rep(1:ceiling(n() / (plots_per_row * plots_per_col)), each = plots_per_row * plots_per_col, length.out = n())) %>%
    group_split()

# Create a list of ggplot objects
ggplot_list <- lapply(plot_list, function(sub_a) {
    ggplot(sub_a, aes(x = listing_month, y = mean_value)) +
        geom_line() +
        labs(title = paste("Neighbourhood Group", unique(sub_a$group)),
             x = "listing_month",
             y = "Mean Value") +
        theme_minimal()
})

# Combine the ggplot objects using multiplot function (you'll need to define multiplot)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

# Display the multiplot
multiplot(plotlist = ggplot_list, cols = plots_per_row)



# Corr matrix -------------------------------------------------------------

library(corrplot)
cols <- c('price', 'house', 'total_area', 'covered_area', 'uncovered_area', 'bedrooms', 'bathrooms', 'rooms', 'property_age', 'pool', 'security', 'furnished', 'heating', 'air_conditioning', 'parking', 'common_space', 'fitness_space', 'distance_to_transport', 'distance_to_greenspace', 'longitude', 'latitude')
cor.matrix <- cor(df[,..cols], use="complete.obs")

# matrix of the p-value of the correlation
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
}
p.mat <- cor.mtest(df[,..cols], use="complete.obs")

correlogram <- corrplot(cor.matrix, method="color", type="lower",
                        addCoef.col = "black",
                        number.cex=0.6,
                        tl.srt=45, tl.col="black",
                        p.mat = p.mat, sig.level = 0.01, insig="pch",
                        diag=T)



cols <- c('price', 'house', 'total_area', 'covered_area', 'uncovered_area', 'bedrooms', 'bathrooms', 'rooms', 'property_age', 'pool', 'security', 'furnished', 'heating', 'air_conditioning', 'parking', 'common_space', 'fitness_space', 'distance_to_transport', 'distance_to_greenspace', 'distance_to_cbd', 'longitude', 'latitude')

# Calculate the correlation coefficients against "price"
correlation_coefficients <- round(cor(df[,..cols])[1, -1],3)

# Create a data frame with the results
correlation_df <- data.frame(variable = names(correlation_coefficients),
                             correlation = correlation_coefficients)
correlation_df$abs_correlation <- abs(correlation_df$correlation)

# Print the correlation data frame
print(correlation_df)



# Ver celdas por barrio y mes ---------------------------------------------

b <- df %>%
         group_by(neighbourhood) %>%
         summarise(total=n()) %>% mutate(mean_n = mean(total)) %>% 
         ungroup()
View(b)

# Ver el barrio con menos obs
neigh_min_obs <- b$neighbourhood[b$total==min(b$total)]
View(df[neighbourhood==neigh_min_obs])
# No hay precios raros

# Ver Nueva pompeya
View(df[neighbourhood=="NUEVA POMPEYA"])
quantile(df$price[df$neighbourhood=="NUEVA POMPEYA"], probs = 0.99)
# Hay precios raros, pero se van con q99 max para toda la muestra
# Ahora habria que ver si se van cuando q99 se hace desde el inicio hasta el mes correspondiente

# Create a sequence of unique months
unique_months <- unique(df$listing_month)

# Calculate q99 of price for all observations up to and including each month
# grouped by neighbourhood and house
q99_results <- lapply(unique_months, function(m) {
    df %>%
        filter(listing_month <= m) %>%
        group_by(neighbourhood, house) %>%
        summarise(q99_price = quantile(price, 
                                       probs = 0.99), 
                  listing_month = m,
                  n=n()
        )
})

# Combine the results into a single data frame
q99_results_df <- bind_rows(q99_results)
rm(q99_results)
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



# percentile without data leakage -----------------------------------------

library(readr)
df <- read_csv("Data/Meli/Clean/CABA/Alquiler/meli_clean_alquiler_2018_2022.csv")

# Load the required library
library(dplyr)

# Iterate through each unique month
unique_months <- unique(df$listing_month)[order(unique(df$listing_month))]

# Initialize an empty data frame to store the results
result_df <- data.frame()

for (month in unique_months) {
    
    # Filter data for the current month and the previous 12 months (including this one)
    month_minus_11 <- as.Date(month, format="%Y-%m-%d") - months(11)
    last_12_months <- unique_months[unique_months >= month_minus_11 & 
                                        unique_months <= month]
    filtered_data <- df %>% filter(listing_month %in% last_12_months)
    
    # Calculate the 99th percentile for each neighborhood and house combination
    min_listings_valid <- 10
    percentile_99 <- filtered_data %>%
        group_by(neighbourhood, house) %>%
        summarise(percentile_99 = quantile(price, 0.99, na.rm = TRUE),
                  n=n()) %>% 
        filter(n>=min_listings_valid) %>% 
        mutate(listing_month = as.Date(month, format="%Y-%m-%d")) %>% 
        relocate(listing_month)
    
    # Add the calculated percentile to the result data frame
    result_df <- bind_rows(result_df, percentile_99)
}

# Join the percentile_99 data back to the original data frame
df <- df %>%
    left_join(result_df, by = c("neighbourhood", "house"))

# Remove the 'listing_month' variable if you don't need it anymore
df <- df %>%
    select(-listing_month)





# Load the required library
library(data.table)

# Read the data using data.table
df <- fread("Data/Meli/Clean/CABA/Alquiler/meli_clean_alquiler_2018_2022.csv")
orig <- df

# Iterate through each unique month
unique_months <- unique(df$listing_month)[order(unique(df$listing_month))]

# Initialize an empty data table to store the results
result_dt_neigh <- data.table()
result_dt_commune <- data.table()
result_dt_commune_flatsandhouses <- data.table()

for (month in unique_months) {
    
    # Filter data for the current month and the previous 12 months (including this one)
    month_minus_11 <- month - months(11)
    last_12_months <- unique_months[unique_months >= month_minus_11 & 
                                        unique_months <= month]
    
    filtered_data <- df[listing_month %in% last_12_months]
    
    # Set minimum listings to consider the percentile valid
    min_listings_valid <- 10
    
    # NEIGHBOURHOODS
    # Calculate the 99th percentile for each neighborhood and house combination
    percentile_99 <- filtered_data[, .(percentile_99_neigh = 
        quantile(price, 0.99, na.rm = TRUE), n = .N),
                                   by = .(neighbourhood, house)]
    percentile_99 <- percentile_99[n >= min_listings_valid]
    percentile_99 <- percentile_99[, n:=NULL]
    
    # Add the calculated percentile to the result data table
    percentile_99[, listing_month := as.IDate(month, format="%Y-%m-%d")]
    result_dt_neigh <- rbindlist(list(result_dt_neigh, percentile_99))
    
    # COMMUNES
    # Calculate the 99th percentile for each neighborhood and house combination
    percentile_99 <- filtered_data[, .(percentile_99_comm = 
        quantile(price, 0.99, na.rm = TRUE), n = .N),
                                   by = .(commune, house)]
    percentile_99 <- percentile_99[n >= min_listings_valid]
    percentile_99 <- percentile_99[, n:=NULL]

    # Add the calculated percentile to the result data table
    percentile_99[, listing_month := as.IDate(month, format="%Y-%m-%d")]
    result_dt_commune <- rbindlist(list(result_dt_commune, percentile_99))
    
    # COMMUNES WITHOUT SEPARATING BY PROPERTY TYPE
    # Calculate the 99th percentile for each neighborhood and house combination
    percentile_99 <- filtered_data[, .(percentile_99_comm2 = 
        quantile(price, 0.99, na.rm = TRUE), n = .N), 
        by = .(commune)]
    percentile_99 <- percentile_99[n >= min_listings_valid]
    percentile_99 <- percentile_99[, n:=NULL]
    
    # Add the calculated percentile to the result data table
    percentile_99[, listing_month := as.IDate(month, format="%Y-%m-%d")]
    result_dt_commune_flatsandhouses <- rbindlist(list(
        result_dt_commune_flatsandhouses, percentile_99))

}

df <- merge(df, result_dt_neigh, by=c("listing_month", "neighbourhood", "house"), 
             all.x = TRUE, all.y = FALSE)
df <- merge(df, result_dt_commune, by=c("listing_month", "commune", "house"), 
             all.x = TRUE, all.y = FALSE)
df <- merge(df, result_dt_commune_flatsandhouses, by=c("listing_month", "commune"), 
             all.x = TRUE, all.y = FALSE)

# Check that there is at least one value for all observations
if(any(is.na(df$percentile_99_comm2))){stop("There are observations without a 99th percentile.")}

rm(filtered_data, percentile_99, 
   result_dt_commune, result_dt_commune_flatsandhouses, result_dt_neigh)

# Remove observations above the corresponding percentile
df <- df[is.na(percentile_99_neigh) | 
             price <= percentile_99_neigh]

df <- df[
    # either the first filter was applied
    !is.na(percentile_99_neigh) | 
        # or it was not applied, but we cannot apply the second filter either
             (is.na(percentile_99_neigh) & is.na(percentile_99_comm)) | 
        # or it was not applied, so apply the second filter
             (is.na(percentile_99_neigh) & price <= percentile_99_comm)] 

df <- df[
    # either the first filter was applied
    !is.na(percentile_99_neigh) | 
        # or the second filter was applied
        !is.na(percentile_99_comm) |
    # or none were applied, so apply the third filter
        # or it was not applied, so apply the second filter
        (is.na(percentile_99_neigh) & is.na(percentile_99_comm) & 
                price <= percentile_99_comm2)] 
    