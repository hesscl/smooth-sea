
#connect to db
DB <- dbConnect(SQLite(), dbname="R:/Project/seattle_rental_market/data/cl/craigslistDB.sqlite")
cl <- tbl(DB, "clean") #clean listing table

#compute tract aggregates for CL listing count
sea_cl <- cl %>%
  filter(!is.na(GISJOIN), seattle == 1) %>% #only listings with valid Bed/Rent
  dplyr::select(listingMoYr, listingDate, GISJOIN, seattle, matchAddress, matchAddress2, matchType, cleanBeds, cleanRent, cleanSqft) %>% #SELECT these columns
  collect %>% #bring db query into memory
  filter(GISJOIN %in% sea_shp@data$GISJOIN) %>% #filter to KC only (db has metro area)
  mutate(listingDate = as.Date(listingDate),
         listingQtr = as.yearqtr(listingDate)) %>%
  filter(listingQtr >= "2017 Q1", !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft),
         cleanBeds == 1) %>% 
  filter(matchType != "Google Maps Lat/Long") %>% #no Google lat/long only geocodes
  distinct(matchAddress, matchAddress2, cleanBeds, cleanRent, cleanSqft, .keep_all = T)
dbDisconnect(DB)

computeCal <- function(x){
  y <- x %>%
    group_by(listingDate) %>%
    summarize(n = n())
  
  fullDates <- seq(from = min(y$listingDate), to = max(y$listingDate), by = 1)
  index <- seq(from = 1, length.out = length(fullDates), by = 1)
  dateTable <- cbind.data.frame(fullDates, index)
  
  n <- NULL
  for(i in 1:length(fullDates)){
    if(fullDates[i] %in% y$listingDate){
      n[i] <- y$n[y$listingDate == fullDates[i]]
    } else{
      n[i] <- NA
    }
  }
  n <- cbind.data.frame(n, fullDates)
}


cal <- computeCal(sea_cl) %>%
  mutate(weekdayf = weekdays(fullDates) %>% as.factor,
         monthf = month(fullDates, label = T),
         yearmon = as.yearmon(fullDates),
         year = year(fullDates),
         week = as.numeric(format(fullDates,"%W")))
sum(is.na(cal$n))/nrow(cal)

cal$weekdayf <- factor(cal$weekdayf, levels = rev(levels(cal$weekdayf)[c(2,6,7,5,1,3,4)]))

#credit to https://rpubs.com/haj3/calheatmap for code and idea
cal<-plyr::ddply(cal,plyr::.(yearmon),transform,monthweek=1+week-min(week))

#make the heatmap
ggplot(cal, aes(x = monthweek, y = weekdayf, fill = n)) +
  scale_fill_viridis_c(na.value = "grey80", direction = 1, option = "A") +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_size(range=c(5,20))+
  geom_tile(color = "white") +
  facet_grid(year~monthf) +
  theme_minimal() +
  theme(axis.text = element_text(size = 6),
        legend.key.size = unit(.1, "inch"),
        legend.position = "bottom",
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.grid = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 11,
                                label.position = "bottom")) +
  xlab("\nWeek of Month") +
  ylab("Day of Week\n") +
  labs(fill = "1B Listings") +
  ggsave(filename = "../output/n_heatmap.png",
         width = 7, height = 3, dpi = 500)

