
#### Helper script to compute map + point density map

#libraries
library(ggmap)
library(patchwork)

#data
DB <- dbConnect(SQLite(), dbname="R:/Project/seattle_rental_market/data/cl/craigslistDB.sqlite")
map_clean <- tbl(DB, "clean") #clean seattle listing table
map_raw <- tbl(DB, "raw")

#load table
map_cl <- map_clean %>% 
  filter(seattle==1) %>%
  collect %>% #bring db query into memory
  filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), 
         GISJOIN %in% sea_shp@data$GISJOIN, matchType != "Google Maps Lat/Long") %>% #only listings with valid Bed/Rent, seattle tracts
  distinct(cleanBeds, cleanRent, cleanSqft, matchAddress, 
           matchAddress2, .keep_all = T) %>% #dedupe to unique address-bed-rent combos
  dplyr::select(listingDate, GISJOIN, seattle, matchAddress, matchType, 
                cleanBeds, cleanRent, cleanSqft, lat, lng) %>% #SELECT these columns
  mutate(listingDate = as.Date(listingDate),
         listingQtr = as.yearqtr(listingDate)) %>%
  filter(cleanBeds %in% c(1), listingQtr >= "2017 Q2")
dbDisconnect(DB)

blank <- qmplot(lng, lat, data = map_cl, maptype = "toner-lite", geom = "blank") 

with_points <- qmplot(lng, lat, data = map_cl, maptype = "toner-lite", darken = .50, legend = "bottom") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon",
                  alpha = .75, color = NA, size = .05) +
  scale_fill_viridis_c("1B Listings", option = "A") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(angle = 45, vjust = .8),
        legend.key.size = unit(.1, "inch"),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 7,
                              label.position = "bottom"))

blank + with_points +
  ggsave(filename = "../output/map_point_density.png",
         width = 6, height = 5.5, units = "in", dpi = 500)


