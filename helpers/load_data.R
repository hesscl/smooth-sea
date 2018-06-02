load_data <- function(){
  #determine config (dev or github)
  if(file.exists("../data/cl/craigslistDB.sqlite")){
    #connect to database if file.exists
    DB <- dbConnect(SQLite(), dbname="../data/cl/craigslistDB.sqlite")
    cl <- tbl(DB, "clean") #create dbi for clean listing table
    
    #compute tract aggregates for CL listing count
    tract <- cl %>%
      collect %>% #bring db query into memory
      filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), 
             GISJOIN %in% sea_shp@data$GISJOIN, #only listings with valid Bed/Rent, seattle tracts
             matchType != "Google Maps Lat/Long") %>% #need to have address, not approximate
      distinct(cleanBeds, cleanRent, cleanSqft, matchAddress, 
               matchAddress2, .keep_all = T) %>% #dedupe to unique address-bed-rent combos
      dplyr::select(listingDate, GISJOIN, seattle, matchAddress, matchType, 
                    cleanBeds, cleanRent, cleanSqft) %>% #SELECT these columns
      mutate(listingDate = as.Date(listingDate),
             listingQtr = as.yearqtr(listingDate)) %>%
      filter(cleanBeds %in% c(0, 1, 2, 3), listingQtr >= "2017 Q2") %>%
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
    write_csv(tract, "./input/tractCl.csv")
    return(tract)
  } else{
    #if github config, read in extract from cl db
    tract <- read_csv("./input/tractCl.csv")
    return(tract)
  }
}