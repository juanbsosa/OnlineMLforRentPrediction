rm(list=ls())
gc()

df <- readxl::read_excel("G:/My Drive/MasterThesis/Data/Economy/TC Blue/dolar_series_raw.xlsx",
                         col_types = c("date", "text", "text"))

sapply(df, class)

colnames(df) <- c('fecha','compra','venta')

df$compra <- stringr::str_replace_all(df$compra, ",", ".")
df$venta <- stringr::str_replace_all(df$venta, ",", ".")

print(paste("From", min(df$fecha), "to", max(df$fecha)))

write.csv(df, "G:/.shortcut-targets-by-id/1cBXxM-2uQkscIdJGsND_EYU7r7IhCegp/Exchange rate passtrough/Data/Extras/TC Blue/dolar_series.csv",
          row.names = F)
