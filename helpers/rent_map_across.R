#### Helper secript to compute across data chloropleth maps

#load census data
rent_map_acs <- read_csv(file = "R:/Project/seattle_rental_market/data/census/acs/nhgis0051_ds226_20165_2016_tract.csv")

#connect to craigslist data
DB <- dbConnect(SQLite(), dbname="R:/Project/seattle_rental_market/data/cl/craigslistDB.sqlite")
rent_map_sea <- tbl(DB, "clean") #clean seattle listing table
rent_map_raw <- tbl(DB, "raw")

#load d+s data
rent_map_ds <- read_csv("R:/Project/seattle_rental_market/data/d+s/geocoded_D+S.csv")

#for background
tract2010_bg <- fortify(tract2010)

#### A. Prep CL table ---------------------------------------------------------

#2017-2018 listings in seattle with non-missing lat/lng, drop 2010 geog fields
cl2017 <- rent_map_sea %>% 
  filter(seattle==1) %>% 
  collect %>% 
  filter(listingYear >= 2017, 
         matchType != "Google Maps Lat/Long",
         !is.na(matchAddress), !is.na(matchAddress2), !is.na(catBeds), !is.na(cleanSqft), !is.na(cleanRent),
         catBeds %in% c("0", "1", "2"), !is.na(lat) & !is.na(lng)) %>%
  distinct(matchAddress, matchAddress2, catBeds, cleanSqft, cleanRent, .keep_all = T) %>%
  group_by(GISJOIN, catBeds) %>%
  summarize(rent = median(cleanRent, na.rm = T),
            nHU = n(),
            data = "Craigslist 2017-2018") %>%
  select(GISJOIN, data, catBeds, rent, nHU)
dbDisconnect(DB)

#### B. Prep D+S table --------------------------------------------------------

#munge starting table of tract summaries
ds2017 <- rent_map_ds %>%
  select(-STATEFP00, -COUNTYFP00, -TRACTCE00, -CTIDFP00, -SHAPE_AREA, -SHAPE_LEN, -NAME00,
         -INTPTLAT00, -INTPTLON00) %>%
  mutate(surveyYear = parse_number(Survey),
         surveySeason = sub("[:0-9:]+", "", Survey),
         surveyMonth = ifelse(surveySeason == "Spring ", 3,
                              ifelse(surveySeason == "Fall ", 9, NA)),
         surveyDate = as.yearmon(paste0(surveyYear, "-0", surveyMonth, "-01")),
         dsRent = Rent) %>% #spring = March, Fall = Sept
  select(-Survey, -surveySeason) %>%
  filter(surveyYear == 2017, surveyMonth == 9, rowType %in% c("Studio", "1BD/1BA", "2BD/1BA", "2BD/2BA")) %>%
  select(GISJOIN, rowType, dsRent, Bldgs, Units)

#studio
ds0BD <- ds2017 %>%
  filter(grepl("Studio", rowType)) %>%
  mutate(catBeds = "0",
         rent = dsRent,
         nHU = Units,
         data = "Dupre+Scott 2017") %>%
  select(GISJOIN, data, catBeds, rent, nHU)

#1BD
ds1BD <- ds2017 %>%
  filter(grepl("1BD", rowType)) %>%
  mutate(catBeds = "1",
         rent = dsRent,
         nHU = Units,
         data = "Dupre+Scott 2017") %>%
  select(GISJOIN, data, catBeds, rent, nHU)

#2BD
ds2BD <- ds2017 %>%
  filter(grepl("2BD", rowType)) %>%
  group_by(GISJOIN) %>%
  summarize(rent = weighted.mean(dsRent, Units),
            catBeds = "2",
            data = "Dupre+Scott 2017", 
            nHU = sum(Units)) %>%
  select(GISJOIN, data, catBeds, rent, nHU)

ds2017 <- bind_rows(ds0BD, ds1BD, ds2BD)

#### C. Prep ACS table -------------------------------------------------

rent_map_acs <- rent_map_acs %>%
  filter(STATE == "Washington", COUNTY == "King County")

acs0B <- rent_map_acs %>%
  mutate(rent = AGPZE002,
         catBeds = "0",
         data = "ACS 2012-2016",
         nHU = NA) %>%
  select(GISJOIN, data, catBeds, rent, nHU)

acs1B <- rent_map_acs %>%
  mutate(rent = AGPZE003,
         catBeds = "1",
         data = "ACS 2012-2016",
         nHU = NA) %>%
  select(GISJOIN, data, catBeds, rent, nHU)

acs2B <- rent_map_acs %>%
  mutate(rent = AGPZE004,
         catBeds = "2",
         data = "ACS 2012-2016",
         nHU = NA) %>%
  select(GISJOIN, data, catBeds, rent, nHU)

data_tract2010 <- bind_rows(acs0B, acs1B, acs2B, cl2017)

tract2010@data$id <- rownames(tract2010@data)
tract2010@data <- left_join(tract2010@data, data_tract2010)

tract2010_f<-fortify(tract2010)
tract2010_f<-left_join(tract2010_f, tract2010@data,"id")

#### D. Fortify table for mapping ---------------------------------------------

#for chloropleth
tract2000@data$id <- rownames(tract2000@data)
tract2000@data <- left_join(tract2000@data, ds2017)

tract2000_f<-fortify(tract2000)
tract2000_f<-left_join(tract2000_f, tract2000@data,"id")

tract_f <- bind_rows(tract2000_f, tract2010_f)

tract_f <- filter(tract_f, !is.na(data))
tract_f$catBeds <- factor(tract_f$catBeds)
levels(tract_f$catBeds) <- c("Studio", "1 Bedroom", "2 Bedrooms")

tract_f$data <- factor(tract_f$data)
tract_f$data <- factor(tract_f$data, levels(tract_f$data)[c(2,3,1)])

#### E. Make plot -------------------------------------------------------------

#1BD
ggplot(tract_f %>% filter(catBeds == "1 Bedroom"), aes(x = long, y = lat, group = group, fill = rent)) +
  facet_grid(~ data) +
  geom_polygon(data = tract2010_bg, aes(x = long, y = lat, group = group), 
               color = "grey90", fill = "grey70", lwd = .15) +
  geom_polygon(color = "grey90", lwd = .15) +
  scale_fill_viridis_c(labels = scales::dollar, na.value = "grey70", option = "A") +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.width = unit(.1, "inch")) +
  labs(fill = "Asking Rent") +
  ggsave(filename = "../output/rent_map_across_1B.png",
         width = 8, height = 4, dpi = 300)