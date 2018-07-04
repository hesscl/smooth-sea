#compute tract aggregates for CL listing count
sea_cl <- load_data(listings = TRUE)

#filter to 1BDs
sea_cl <- sea_cl %>% 
  filter(cleanBeds == 1)

#function to compute the daily ts for n with missingness allowed
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

#compute the daily ts of n listings, rework fields to what is needed for gg
cal <- computeCal(sea_cl) %>%
  mutate(weekdayf = weekdays(fullDates) %>% as.factor,
         monthf = month(fullDates, label = T),
         yearmon = as.yearmon(fullDates),
         year = year(fullDates),
         week = as.numeric(format(fullDates,"%W")))
sum(is.na(cal$n))/nrow(cal)

#order the days of week to match graphic format
cal$weekdayf <- factor(cal$weekdayf, levels = rev(levels(cal$weekdayf)[c(2,6,7,5,1,3,4)]))

#credit to https://rpubs.com/haj3/calheatmap for code and idea
cal <- plyr::ddply(cal, plyr::.(yearmon), transform, monthweek = 1+week-min(week))

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
  guides(fill = guide_colourbar(barwidth = 11, label.position = "bottom")) +
  xlab("\nWeek of Month") +
  ylab("Day of Week\n") +
  labs(fill = "1B Listings") +
  ggsave(filename = "../output/n_heatmap.png",
         width = 7, height = 3, dpi = 300)

