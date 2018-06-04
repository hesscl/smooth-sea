
### Helper script to produce across data lineplot of median rent for Seattle

#data
DB <- dbConnect(SQLite(), dbname="R:/Project/seattle_rental_market/data/cl/craigslistDB.sqlite")
sea <- tbl(DB, "clean") #clean seattle listing table
raw <- tbl(DB, "raw")

#for code that has not been updated to db calls
cl <- sea %>% collect %>% filter(seattle==1)

#load Seattle market summaries from D+S
ds <- read_csv(file = "R:/Project/seattle_rental_market/data/d+s/city-level D+S.csv")


#### A. Prepare a table for ggplot ---------------------------------------------

sumCL <- cl %>%
  #mutate(cleanRent = ifelse(cleanRent > 5000, NA, cleanRent)) %>%
  mutate(catSqft = fct_reorder(catSqft, cleanSqft)) %>%
  arrange(listingDate) %>%
  select(-ends_with("10")) %>%
  filter(!is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), !is.na(matchAddress), !is.na(matchAddress2),
         matchType != "Google Maps Lat/Long") %>%
  distinct(matchAddress, matchAddress2, catBeds, cleanSqft, cleanRent, .keep_all = T) %>%
  mutate(listingQtr = as.yearqtr(as.Date(listingDate))) %>%
  group_by(listingQtr) %>%
  filter(listingQtr >= "2017 Q1") %>%
  summarize(Rent = median(cleanRent, na.rm=T),
            nHU = n()) %>%
  rename(moYr = listingQtr) %>%
  mutate(dataSrc = "Craigslist") %>%
  select(moYr, Rent, dataSrc) %>%
  mutate(moYr = as.Date(moYr))

sumDS <- ds %>%
  filter(rowType == "All HUs") %>%
  mutate(surveyYear = parse_number(Survey),
         surveySeason = sub("[:0-9:]+", "", Survey),
         surveyMonth = ifelse(surveySeason == "Spring ", 3,
                              ifelse(surveySeason == "Fall ", 9, NA)),
         surveyDate = as.yearmon(paste0(surveyYear, "-0", surveyMonth, "-01"))) %>% #spring = March, Fall = Sept
  select(-Survey, -surveySeason) %>%
  mutate(Rent = ifelse(surveyYear == 1997, Rent * 1.56, Rent),
         Rent = ifelse(surveyYear == 1998, Rent * 1.53, Rent),
         Rent = ifelse(surveyYear == 1999, Rent * 1.51, Rent),
         Rent = ifelse(surveyYear == 2000, Rent * 1.47, Rent),
         Rent = ifelse(surveyYear == 2001, Rent * 1.42, Rent),
         Rent = ifelse(surveyYear == 2002, Rent * 1.40, Rent),
         Rent = ifelse(surveyYear == 2003, Rent * 1.36, Rent),
         Rent = ifelse(surveyYear == 2004, Rent * 1.34, Rent),
         Rent = ifelse(surveyYear == 2005, Rent * 1.30, Rent),
         Rent = ifelse(surveyYear == 2006, Rent * 1.25, Rent),
         Rent = ifelse(surveyYear == 2007, Rent * 1.22, Rent),
         Rent = ifelse(surveyYear == 2008, Rent * 1.17, Rent),
         Rent = ifelse(surveyYear == 2009, Rent * 1.17, Rent),
         Rent = ifelse(surveyYear == 2010, Rent * 1.14, Rent),
         Rent = ifelse(surveyYear == 2011, Rent * 1.13, Rent),
         Rent = ifelse(surveyYear == 2012, Rent * 1.09, Rent),
         Rent = ifelse(surveyYear == 2013, Rent * 1.08, Rent),
         Rent = ifelse(surveyYear == 2014, Rent * 1.06, Rent),
         Rent = ifelse(surveyYear == 2015, Rent * 1.06, Rent),
         Rent = ifelse(surveyYear == 2016, Rent * 1.05, Rent),
         Rent = ifelse(surveyYear == 2017, Rent * 1.02, Rent)) %>%
  mutate(surveyDate = as.Date(surveyDate),
         dataSrc = "Dupre+Scott") %>%
  rename(moYr = surveyDate) %>%
  select(moYr, Rent, dataSrc)

#obtained via american fact finder, not enough time to compile table via NHGIS origins
sumACS <- data.frame(
  "moYr" = as.Date(c("2005-06-01", "2006-06-01", "2007-06-01", "2008-06-01", "2009-06-01", 
                     "2010-06-01", "2011-06-01", "2012-06-01", "2013-06-01", "2014-06-01",
                     "2015-06-01", "2016-06-01")),
  "dataSrc" = rep("ACS 1YR", 12),
  "Rent" = c(804, 833, 881, 940, 992, 990, 1024, 1072, 1172, 1202, 1356, 1448)
)

sumACS <- sumACS %>%
  mutate(surveyYear = year(moYr)) %>%
  mutate(Rent = ifelse(surveyYear == 2005, Rent * 1.30, Rent),
         Rent = ifelse(surveyYear == 2006, Rent * 1.25, Rent),
         Rent = ifelse(surveyYear == 2007, Rent * 1.22, Rent),
         Rent = ifelse(surveyYear == 2008, Rent * 1.17, Rent),
         Rent = ifelse(surveyYear == 2009, Rent * 1.17, Rent),
         Rent = ifelse(surveyYear == 2010, Rent * 1.14, Rent),
         Rent = ifelse(surveyYear == 2011, Rent * 1.13, Rent),
         Rent = ifelse(surveyYear == 2012, Rent * 1.09, Rent),
         Rent = ifelse(surveyYear == 2013, Rent * 1.08, Rent),
         Rent = ifelse(surveyYear == 2014, Rent * 1.06, Rent),
         Rent = ifelse(surveyYear == 2015, Rent * 1.06, Rent),
         Rent = ifelse(surveyYear == 2016, Rent * 1.05, Rent),
         Rent = ifelse(surveyYear == 2017, Rent * 1.02, Rent)) %>%
  select(-surveyYear)

sumRent <- bind_rows(sumCL, sumDS, sumACS)

#### B. Make the graphic ------------------------------------------------------

sumRent$dataSrc <- factor(sumRent$dataSrc)
sumRent$dataSrc <- factor(sumRent$dataSrc, levels = levels(sumRent$dataSrc)[c(2, 3, 1)])

#all HUs median rent trend 1997-2017 actual
ggplot(sumRent, aes(x = moYr, y = Rent, group = dataSrc, color = dataSrc, shape = dataSrc)) +
  geom_line(lwd = 1.0, alpha = .5) +
  geom_point(size = 1.5) +
  scale_color_viridis_d(begin = .1, end = .9) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(limits = c(min(sumRent$moYr), max(sumRent$moYr))) +
  theme_minimal() +
  xlab("\nYear") +
  ylab("Median Rent\n") +
  labs(color = "Data Source",
       shape = "Data Source") +
  ggsave(file = "../output/rent_lineplot.png",
         device = "png", width = 6, height = 4)
