
#### Helper script to compute map + point density map

#libraries
library(ggmap)
library(patchwork)

#load table
map_cl <- load_data(listings = TRUE)

#filter to 1BD listings
map_cl <- map_cl %>%
  filter(cleanBeds == 1)

#create left panel
blank <- qmplot(lng, lat, data = map_cl, maptype = "toner-lite", geom = "blank") 

#create right panel
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

#append panels together and save
blank + with_points +
  ggsave(filename = "../output/map_point_density.png",
         width = 6, height = 5.5, units = "in", dpi = 300)