
# TO-DOs
# !!! A named list would be much better than this approach


# List --------------------------------------------------------------------

return_list <- function(){
        
    dpto_mapping_caba <- list(
        comuna_1 = c("CONSTITUCION", "MONTSERRAT", "PUERTO MADERO", "RETIRO", 
                     "SAN NICOLAS", "SAN TELMO", "MONSERRAT"),
        comuna_2 = c("RECOLETA"),
        comuna_3 = c("BALVANERA", "SAN CRISTOBAL", "ONCE", "CONGRESO"), # Congreso es el barrio alrededor del Congreso de la Nación
        comuna_4 = c("BARRACAS", "LA BOCA", "NUEVA POMPEYA", "PARQUE PATRICIOS", "BOCA"),
        comuna_5 = c("ALMAGRO", "BOEDO"),
        comuna_6 = c("CABALLITO", "PARQUE CENTENARIO"),
        comuna_7 = c("FLORES", "PARQUE CHACABUCO"),
        comuna_8 = c("VILLA LUGANO", "VILLA RIACHUELO", "VILLA SOLDATI"),
        comuna_9 = c("LINIERS", "MATADEROS", "PARQUE AVELLANEDA"),
        comuna_10 = c("FLORESTA" , "MONTE CASTRO", "VELEZ SARSFIELD", "VERSALLES", "VILLA LURO", "VILLA REAL"),
        comuna_11 = c("VILLA DEL PARQUE", "VILLA DEVOTO", "VILLA GRAL. MITRE",
                      "VILLA GENERAL MITRE",
                      "VILLA SANTA RITA", "SANTA RITA"),
        comuna_12 = c("COGHLAN", "SAAVEDRA", "VILLA PUEYRREDON", "VILLA URQUIZA"),
        comuna_13 = c("BELGRANO", "COLEGIALES", "NUNEZ", "BELGRANO BARRANCAS", 
                      "BELGRANO C", "BELGRANO R", "BELGRANO CHICO"),
        comuna_14 = c("PALERMO", "LAS CANITAS", "PALERMO CHICO", "PALERMO VIEJO", 
                      "PALERMO SOHO", "BOTANICO", "PALERMO NUEVO", "PALERMO HOLLYWOOD"),
        comuna_15 = c("AGRONOMIA", "CHACARITA", "PARQUE CHAS", "PATERNAL", "VILLA CRESPO", "VILLA ORTUZAR")
    )
    
    return(dpto_mapping_caba)
    
}

