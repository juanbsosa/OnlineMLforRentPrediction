#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#                   MASTER SCRIPT                   #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""


# Description: This script is used to run all scripts that are required for the 
#   entire analysis in R.

rm(list=ls())
gc()

# Define directories ------------------------------------------------------

# Master directory
google_drive_letter <- "G"
master_dir <- paste0(google_drive_letter, ":/My Drive/MasterThesis")
setwd(master_dir)

# Directory with all scripts
code_dir <- paste(master_dir, "/Code", sep='')


# Necessary libraries for the project -------------------------------------

# List of all required packages
required_packages <- c(
    "glue", "readr", "data.table", "dplyr", "psych", "stringr", "stringi",
    "sf", "mapview", "ggplot2", "lubridate", "pryr", "nngeo", "tictoc", "beepr"
)

# List of the packages that are not already installed
not_installed_packages <- required_packages[!required_packages %in% installed.packages()]

# Install these packages
for(lib in not_installed_packages) install.packages(lib,dependencies=TRUE)


# Define R settings -------------------------------------------------------

# Avoid scientific notation
options(scipen=999)

# Allow more prints in console
options(max.print=10000000)

# Allow full width of console
# options("width"=200)

# Enable/disable debugger when encountering error
options(error = recover)
# options(error = stop)

# Save console output to file
cotf <- F

# Set seed
set.seed(789)


# # Clean data --------------------------------------------------------------
# 
# tictoc::tic()
# 
# source(file.path(code_dir, "1.1.JoinMonthlyData.R"))
# source(file.path(code_dir, "1.2.CleanIncorrectData.R"))
# source(file.path(code_dir, "1.3.CleanDataByLocation.R"))
# source(file.path(code_dir, "1.4.AddVariables.R"))
# 
# # Write console output to log
# if(cotf){
#     path_ <- getwd()
#     directories <- c("Output", "Log")
#     for (dir_name in directories) {
#         path_ <- file.path(path_, dir_name)
#         if (!dir.exists(path_)) {
#             dir.create(path_)
#         }
#     }
#     sink(file.path(path_, "master_output_log.txt"), append = FALSE, split=TRUE)
# }
# 
# operation <- "ALQUILER"
# # operations <- c("ALQUILER", "VENTA")
# years <- 2017:2022
# # years <- 2018:2019
# 
# for(year in years){
# 
#     verbose_ <- T
# 
#     df <- join_monthly_dbs(year=year, browse=F, max_rows=Inf,
#                            starting_month=1, plot_checks=T)
#     df <- clean_data(df, year=year, browse=F, verbose=verbose_, plot_checks=T,
#                      operations=operation)
#     df <- clean_data_geo(df, year=year, browse=F, verbose=verbose_, plot_checks=T)
#     # df <- clean_outliers(df, year=year, browse=F, verbose=verbose_, plot_checks=T)
#     df <- add_variables(df, year=year, browse=F, verbose=verbose_)
# 
#     assign(paste0("df", year), df, envir = parent.frame())
#     rm(df)
#     gc()
# 
# }
# 
# # Bind dfs
# df_objects <- grep("^df[0-9]{4}$", ls(), value = TRUE)
# df <- do.call(rbind, lapply(df_objects, get))
# # df <- rbind(df2018, df2019, df2020, df2021, df2022)
# rm(list = ls(pattern = "^df[0-9]{4}$"))
# gc()
# 
# # Save DF
# save <- T
# aux_csv <- F
# if(save){
#     path_ <- file.path(getwd(), "Data")
#     directories <- c("Meli", "Clean", "CABA", stringr::str_to_title(operation))
#     for (dir_name in directories) {
#         path_ <- file.path(path_, dir_name)
#         if (!dir.exists(path_)) {
#             dir.create(path_)
#         }
#     }
#     path_ <- file.path(path_, paste0("meli_preclean_",  tolower(operation), "_",
#                                      years[1], "_", years[length(years)]))
#     if(aux_csv){
#         path_ <- paste0(path_, "_aux.csv")
#     } else{
#         path_ <- paste0(path_, ".csv")
#     }
# 
#     write.csv(df, path_, row.names = FALSE)
# }
# 
# 
# # Restore output to console
# if(cotf){sink()}
# 
# tictoc::toc()
# beepr::beep()
# 
# 
# 
# Descriptive plots and tables --------------------------------------------

# Option to load the data frame from the file
operation <- "ALQUILER"
years <- 2017:2022
path_ <- file.path(getwd(), "Data", "Meli", "Clean", "CABA",
            stringr::str_to_title(operation),
            paste0("meli_preclean_",  tolower(operation), "_", years[1], "_",
                   years[length(years)], ".csv"))
df <- data.table::fread(path_, encoding = "UTF-8",nrows=Inf)

# Impute missing values (for age)
source(file.path(code_dir, "2.1.MissingValueImputation.R"))
df <- impute_missings(df, browse = F)

# Remove price per m2 outliers
source(file.path(code_dir, "2.2.RemoveOutliers.R"))
df <- remove_outliers(df, browse = F)

# Remove observations from 2017
years <- 2018:2022
df <- df[format(df$listing_month, "%Y") %in% years, ]

# Keep only one observation per listing (at random)
set.seed(789)
df <- df[, .SD[sample(.N, 1)], by = id]

# Save DF
save <- T
if(save){
    path_ <- file.path(getwd(), "Data", "Meli", "Clean", "CABA", 
                stringr::str_to_title(operation), 
                paste0("meli_clean_",  tolower(operation), "_", years[1], "_", 
                       years[length(years)], ".csv"))
    write.csv(df, path_, row.names = FALSE)
}

# Option to load the data frame from the file
operation <- "ALQUILER"
years <- 2018:2022
path_ <- file.path(getwd(), "Data", "Meli", "Clean", "CABA", 
                   stringr::str_to_title(operation), 
                   paste0("meli_clean_",  tolower(operation), "_", years[1], "_", 
                          years[length(years)], ".csv"))
df <- data.table::fread(path_, encoding = "UTF-8",nrows=Inf)

source(file.path(code_dir, "2.4.SummaryStatistics.R"))
create_stats_tables(df, browse = F, years=years)

source(file.path(code_dir, "2.5.ExploratoryPlots.R"))
create_plots(df, browse = T, years=years, show_plot=T)


