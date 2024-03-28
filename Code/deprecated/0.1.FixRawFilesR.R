# Some raw files with the Meli dataset have errors. Fix them before starting
#   their cleaning process.

library(dplyr)

# 1) Files characters -------------------------------------------
paths <- c(
    "Data/Meli/Raw/AMBA/2020/REALESTATE_MLA_AMBA_202009.csv"
    # "Data/Meli/Raw/AMBA/2020/REALESTATE_MLA_AMBA_202010.csv",
    # "Data/Meli/Raw/AMBA/2020/REALESTATE_MLA_AMBA_202011.csv"
    )

for(path_ in paths){
    
    # Read the CSV file as a single chunk of text
    csv_text <- readLines(file.path(getwd(), path_))

    # Change specific characters
    csv_text <- gsub('\"\"', '\"', csv_text)
    if(path_=="Data/Meli/Raw/AMBA/2020/REALESTATE_MLA_AMBA_202009.csv"){
        csv_text <- gsub('\"2020-09-01,', '\"2020-09-01\",', csv_text)
        csv_text <- gsub('Parcela, nicho y bóveda', 
                         'Parcela nicho y bóveda', csv_text)
        csv_text <- gsub('Parcela, Nicho y Boveda', 
                         'Parcela nicho y bóveda', csv_text)
        csv_text <- gsub('Parcelas, Nichos y Bóvedas', 
                         'Parcela nicho y bóveda', csv_text)
        
    }

    # Write the modified text to a new CSV file
    writeLines(csv_text, path_)

}

# # reo que lo solucione para 09 pero no para 10 y 11
# path_ <- "Data/Meli/Raw/AMBA/2020/REALESTATE_MLA_AMBA_202009.csv"
# a <- read.csv(file=file.path(getwd(), path_), header=T, sep=',', quote='""')
# 
# df_aux <- fread(file.path(getwd(), path_), encoding = "UTF-8", 
#                 nrows=Inf)
