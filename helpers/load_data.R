load_data <- function(listings = FALSE){
  #determine config (dev or github)
  if(file.exists("../../data/cl/craigslistDB.sqlite")){
    #connect to database if file.exists
    DB <- dbConnect(SQLite(), dbname="../../data/cl/craigslistDB.sqlite")
    cl <- tbl(DB, "clean") #create dbi for clean listing table
    
    listing <- cl %>%
      collect %>% #bring db query into memory
      filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), !is.na(matchAddress), !is.na(matchAddress2),
             GISJOIN %in% sea_shp@data$GISJOIN, #only listings with valid Bed/Rent, seattle tracts
             matchType != "Google Maps Lat/Long", #need to have address, not approximate
             cleanBeds %in% c(0, 1, 2, 3)) %>% 
      distinct(cleanBeds, cleanRent, cleanSqft, matchAddress, 
               matchAddress2, .keep_all = T) %>% #dedupe to unique address-bed-rent combos
      dplyr::select(listingDate, GISJOIN, seattle, matchAddress, matchType, 
                    cleanBeds, catBeds, cleanRent, cleanSqft, lat, lng) %>% #SELECT these columns
      mutate(listingDate = as.Date(listingDate),
             listingQtr = as.yearqtr(listingDate)) %>%
      filter(listingQtr >= "2017 Q1", listingQtr < "2018 Q3")
    
    #return table of listings if arg listings == TRUE
    if(listings){
      dbDisconnect(DB)
      return(listing) 
    } #otherwise return tract estimates
    else if(!listings){
      tract <- listing %>%
        group_by(GISJOIN, listingQtr) %>% #group listings by tract, qtr within tract
        summarize(nListings = n(),
                  n1B = sum(cleanBeds == 1),
                  medRent = median(cleanRent),
                  med0B = median(cleanRent[cleanBeds==0]),
                  med1B = median(cleanRent[cleanBeds==1]),
                  med2B = median(cleanRent[cleanBeds==2]),
                  med3B = median(cleanRent[cleanBeds==3])) %>% #create tract aggregates
        ungroup %>%
        arrange(GISJOIN, listingQtr)
      dbDisconnect(DB)
      return(tract)
    }

  } else{
    #if github config
    print("Need input data")
  }
}
