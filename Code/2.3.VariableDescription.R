#""""""""""""""""""""""""""""""""""""""""""""""""""""
#                                                   #
#         TABLE WITH VARIABLE DESCRIPTION           #
#                                                   #
#""""""""""""""""""""""""""""""""""""""""""""""""""""

library(xtable)

create_var_description <- function(
        browse=F
){
    
    if(browse){browser()}

    # Variables (not all) and their description
    descriptions <- list(
      id = 'Identicication number for each unique listing',
      listing_month = 'Date of publication',
      listing_age = 'Number of days since start of publication',
      month = 'Month of the year of publication (1 to 12)',
      invoiced_in_usd = 'Binary equal to 1 if listing was published invoiced in US dollars',
      price_realpesos = 'Rent asking price in real pesos of December 2022',
      price_usd = 'Rent asking price in current US dollars',
      total_area = 'Total property area in square meters',
      covered_area = 'Covered property area in square meters',
      uncovered_area = 'Uncovered property area in square meters',
      price_realpesos_per_covered_sqm = 'Price in real pesos per covered square meter',
      price_usd_per_covered_sqm = 'Price in US dollars per covered square meter',
      house = 'Binary equal to 1 if property is a house, 0 if it is a flat',
      rooms = 'Number of rooms',
      bedrooms = 'Number of bedrooms',
      bathrooms = 'Number of bathrooms',
      furnished = 'Binary equal to 1 if property is offered as furnished',
      property_age = 'Property age in years',
      heating = 'Binary equal to 1 if property has a heating system',
      air_conditioning = 'Binary equal to 1 if property has air conditioning',
      parking = 'Binary equal to 1 if property or building has parking',
      security = 'Binary equal to 1 if property or building has private security',
      pool = 'Binary equal to 1 if property or building has swimming pool',
      common_space = 'Binary equal to 1 if property or building has at least one of the following ammenities: multi-purpose room, party room, playground, business center, grill area, cinema',
      fitness_space = 'Binary equal to 1 if property has at least one of the following ammenities: gym, tennis court, jogging track',
      latitude = 'Latitude in decimal degrees (EPSG 4326)',
      longitude = 'Longitude in decimal degrees (EPSG 4326)',
      commune = 'Official administrative area in which the property is located',
      neighbourhood = 'Unofficial neighbourhood in which the property is located',
      distance_to_transport = 'Distance to nearest subway or train station in meters',
      distance_to_greenspace = 'Distance to nearest green space in meters'
    )
    
    # Make it tidy
    variable_description <- as.data.frame(t(as.data.frame(descriptions)))
    variable_description$Variable <- rownames(variable_description)
    names(variable_description)[1] <- 'Description'
    variable_description <- variable_description[,c('Variable', 'Description')]
    rownames(variable_description) <- NULL
    
    #Export as latex table
    xtab <- xtable(variable_description, 
           caption="Variable description", 
           label="tab:var_description")
    print(xtab, include.rownames=FALSE)
    
    # !!! Then the table needs to be fixed to wrap text (with \makecell[l] and \\)
    # and to adjust the width to one page using \resizebox{\textwidth}{!}
    # \renewcommand{\arraystretch}{1.1} to increase row height
}
