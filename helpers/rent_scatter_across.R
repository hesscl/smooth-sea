#### Helper script to compute scatter plot of CL X D+S

#data
DB <- dbConnect(SQLite(), dbname="R:/Project/seattle_rental_market/data/cl/craigslistDB.sqlite")
scatter_sea <- tbl(DB, "clean") #clean seattle listing table
scatter_raw <- tbl(DB, "raw")

#read in D+S tract summaries
scatter_ds <- read_csv("R:/Project/seattle_rental_market/data/d+s/geocoded_D+S.csv")

#### A. Prepare a table for ggplot ---------------------------------------------

scatter_cl <- scatter_sea %>% 
  filter(seattle==1) %>%
  collect %>% #bring db query into memory
  filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), 
         GISJOIN %in% tract2000@data$GISJOIN, matchType != "Google Maps Lat/Long") %>% #only listings with valid Bed/Rent, seattle tracts
  distinct(cleanBeds, cleanRent, cleanSqft, matchAddress, 
           matchAddress2, .keep_all = T) %>% #dedupe to unique address-bed-rent combos
  dplyr::select(listingYear, listingDate, GISJOIN, seattle, matchAddress, matchType, 
                cleanBeds, cleanRent, cleanSqft, lat, lng) %>% #SELECT these columns
  mutate(listingDate = as.Date(listingDate),
         listingQtr = as.yearqtr(listingDate)) %>%
  filter(cleanBeds %in% c(1), listingQtr >= "2017 Q1")

coordinates(scatter_cl) <- cbind(scatter_cl$lng, scatter_cl$lat)

#make sure shapefile has correct projection
WGS.latlon <- CRS("+proj=longlat +datum=WGS84")
tract2000 <- readOGR(dsn = "../input/sea_tract_2000/sea_tract_2000.shp",
                     layer = "sea_tract_2000",
                     GDAL1_integer64_policy = TRUE,
                     verbose = FALSE,
                     stringsAsFactors = F) 
tract2000 <- spTransform(tract2000, WGS.latlon)
proj4string(scatter_cl) <- WGS.latlon

#append 2000 geography to listings
GISJOIN <- over(scatter_cl, tract2000[,"GISJOIN"])
scatter_cl <- bind_cols(scatter_cl@data, GISJOIN)

#compute summaries
ds2017 <- scatter_ds %>%
  select(-STATEFP00, -COUNTYFP00, -TRACTCE00, -CTIDFP00, -SHAPE_AREA, -SHAPE_LEN, -NAME00,
         -INTPTLAT00, -INTPTLON00) %>%
  filter(rowType == "1BD/1BA") %>%
  mutate(surveyYear = parse_number(Survey),
         surveySeason = sub("[:0-9:]+", "", Survey),
         surveyMonth = ifelse(surveySeason == "Spring ", 3,
                              ifelse(surveySeason == "Fall ", 9, NA)),
         surveyDate = as.yearmon(paste0(surveyYear, "-0", surveyMonth, "-01")),
         dsRent = Rent) %>% #spring = March, Fall = Sept
  select(-Survey, -surveySeason) %>%
  filter(surveyYear == 2017, surveyMonth == 9, rowType == "1BD/1BA") %>%
  select(GISJOIN, dsRent, Bldgs, Units)

cl2017 <- scatter_cl %>%
  group_by(GISJOIN) %>%
  summarize(clRent = median(cleanRent[cleanBeds==1], na.rm =T),
            nHU = n()) %>%
  #filter(nHU >= 15) %>%
  select(GISJOIN, clRent, nHU)

across2017 <- left_join(tract2000@data, cl2017)
across2017 <- left_join(across2017, ds2017)
pal <- brewer.pal(3, "Purples")

ggplot(across2017, aes(x = dsRent, y = clRent, size = nHU)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(show.legend = FALSE, se=FALSE, linetype = 5, 
              color = viridis(3, begin = .25, option = "A")[1], lwd = 1.25) +
  theme_minimal() +
  coord_cartesian() +
  scale_x_continuous(limits = c(800, 2500), labels = scales::dollar) +
  scale_y_continuous(limits = c(800, 2500), labels = scales::dollar) +
  scale_size_area(max_size = 6) +
  xlab("\nDupre+Scott September 2017 Median 1B Rent") +
  ylab("Craigslist 2017-2018 Median 1B Rent\n") +
  labs(size = "N Craigslist\nlistings") +
  ggsave(file = "../output/rent_scatter_across_1B.png",
         width = 6, height = 4, dpi = 300)
