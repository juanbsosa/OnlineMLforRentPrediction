#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#              EXPLORATORY ANALYSIS                 #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(data.table)
library(psych)
library(dplyr)
library(openxlsx)
library(xtable)

# TO-DOs
# Add option to assign tables

# Function -----------------------------------------------------------------

create_stats_tables <- function(
        df,
        years,
        browse=F
){
    
    if(browse){browser()}
    
    # CREATE TABLE WITH SUMMARY STATISTICS
    
    create_stats <- function(df, dec_places=2){

        # Create
        stats_table <- describe(df, na.rm=F, skew=T, check=F, quant=c(.25,.75))
        # Round
        stats_table <- round(stats_table, dec_places)
        # Tidy
        stats_table$vars <- rownames(stats_table)
        names(stats_table)[which(names(stats_table)=="vars")] <- "variable"
        cols <- c("variable", "min", "Q0.25", "median", "mean", "Q0.75", "max", "sd")
        stats_table <- stats_table[,cols]
        rm(cols)
        stats_table[stats_table$vars %in% binary, c("Q0.25", "median", "Q0.75", "sd")] <- NA
        
        return(stats_table)
        
    }
    
    # Create an Excel workbook
    wb <- createWorkbook()
    
    # ALL THE DATA
    numeric <- c("listing_age", 
                 "total_area", "covered_area", "uncovered_area", 
                 "price_realpesos", "price_usd",
                 "price_realpesos_per_covered_sqm", "price_usd_per_covered_sqm",
                 "rooms", "bedrooms", "bathrooms",
                 "property_age",  
                 "distance_to_transport", #"distance_to_cbd",
                 "distance_to_greenspace")
    binary <- c("invoiced_in_usd",
                "house", "furnished",
                "heating", "air_conditioning", 
                "parking", "security", "pool",
                "common_space", "fitness_space")
    vars_to_stat <- c(numeric, binary)
    
    print("Calculating summary statistics for the entire data...")
    cat("\n")
    s_table_all <- create_stats(df=df[, ..vars_to_stat])
    
    # Save to Excel
    addWorksheet(wb, sheetName = "All")
    writeData(wb, sheet = "All", x = s_table_all)
    
    # FOR PROPERTY TYPE
    for(house_ in unique(df$house)){
        
        ptype <- ifelse(house_==0, "flats", "houses")
        
        print(glue::glue("Calculating summary statistics for {ptype}..."))
        cat("\n")
        s_table_ptype <- create_stats(df=df[house==house_, ..vars_to_stat])
        
        # Save to Excel
        addWorksheet(wb, sheetName = ptype)
        writeData(wb, sheet = ptype, x = s_table_ptype)
        
        # FOR EACH YEAR
        for(year_ in unique(df$year)){

            print(glue::glue("Calculating summary statistics for {ptype} and year {year_}..."))
            cat("\n")
            s_table_ptype_y <- create_stats(df=df[house==house_ & year==year_, ..vars_to_stat])
            # assign(paste0("stats_table_ptype_", ptype, "_", year), s_table_ptype_y,
                   # envir = parent.frame())
            
            # Save to Excel
            addWorksheet(wb, sheetName = paste0(ptype, year_))
            writeData(wb, sheet = paste0(ptype, year_), x = s_table_ptype_y)

        }
        
    }
    
    # Save the Excel workbook
    path_ <- file.path(getwd(), "Output")
    directories <- c("Tables", "SummaryStatistics")
    for (dir_name in directories) {
        path_ <- file.path(path_, dir_name)
        if (!dir.exists(path_)) {
            dir.create(path_)
        }
    }
    file_name <- paste0("summary_statistics_",  tolower(operation), "_", years[1], "_", 
                        years[length(years)], ".xlsx")
    saveWorkbook(wb, file = file.path(path_, file_name),
                 overwrite = T)
    rm(file_name, path_)
    
    # Export tables as LaTeX code
    xtab <- xtable(s_table_all, 
                   caption="Summary statistics", 
                   label="tab:sum_stats")
    print(xtab, include.rownames=FALSE)
}