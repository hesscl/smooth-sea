#### INLA Rent Maps for CL ----------------------------------------------------
#### CSSS 512

#### Preamble -----------------------------------------------------------------

#packages
library(tidyverse)
library(forcats)
library(haven)
library(lubridate)
library(haven)
library(zoo)
library(ggthemes)
library(RColorBrewer)
library(sp)
library(spdplyr)
library(geosphere)
library(rgdal)
library(sqldf)
library(spdep)
library(viridis)
library(latex2exp)

#setwd to location of file (REQUIRES RSTUDIO)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load ACS data extract (codebook in folder)
census <- read_csv(file = "./input/acsExtract.csv")

#read in tract shapefiles for seattle
sea_shp <- readOGR(dsn = "R:/Project/seattle_rental_market/data/geo/sea_tract_2010/sea_tract_2010.shp",
                  layer = "sea_tract_2010",
                  GDAL1_integer64_policy = TRUE,
                  stringsAsFactors = F)

#determine config (dev or github)
if(file.exists("../data/cl/craigslistDB.sqlite")){
  #connect to database if file.exists
  DB <- dbConnect(SQLite(), dbname="../data/cl/craigslistDB.sqlite")
  cl <- tbl(DB, "clean") #create dbi for clean listing table
  
  #compute tract aggregates for CL listing count
  tract <- cl %>%
    collect %>% #bring db query into memory
    filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft), 
           GISJOIN %in% sea_shp@data$GISJOIN) %>% #only listings with valid Bed/Rent, seattle tracts
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
              med3B = median(cleanRent[cleanBeds==3]),
              lagRent = lag(cleanRent)) %>% #create tract aggregates
    ungroup %>%
    arrange(GISJOIN, listingQtr)
  dbDisconnect(DB)
  write_csv(tract, "./input/tractCl.csv")
  
} else{
  #if github config, read in extract from cl db
  tract <- read_csv("./input/tractCl.csv")
}


#### A. construct panel (some missingness) ------------------------------------

#ensure order of census df matches shapefile
census <- census[match(sea_shp@data$GISJOIN, census$GISJOIN),]

#clean a few fields we might use
census <- census %>%
  mutate(nHU = AF7PE001,
         pOwnoccHU = AF7PE002/AF7PE001,
         medHUVal = AF9LE001)

#store NULL object to bind iterated dfs into
panel <- NULL

#for each unique quarter in the CL data
for(i in unique(as.character(tract$listingQtr))){
  
  period <- tract %>% #start pipe with tract, end with period
    mutate(listingQtr = factor(listingQtr, ordered = T)) %>% #make factor
    filter(listingQtr == i) %>% #filter to qtr i
    right_join(census) %>% #right join to census, allows missingness
    ungroup() %>% #remove grouping by tract
    mutate(listingQtr = i, #listingQtr == current iteration
           Qtr = gsub(pattern = "Q\\d", replacement = "", x = listingQtr), #subset to QX
           actualRent = med1B) #actual for actual versus predicted
  panel <- bind_rows(panel, period) #append to panel object
}

sea_df <- panel %>% #set to NA for INLA to forecast
  mutate(med1B = ifelse(listingQtr == as.yearqtr(Sys.Date()), NA, med1B),
         med1B = ifelse(n1B < 5, NA, med1B),
         actualRent = med1B)%>%
  dplyr::select(GISJOIN, listingQtr, Qtr, medRent, med1B, lagRent, actualRent, nHU,
                n1B, nListings, pOwnoccHU, medHUVal, idtract) %>%
  arrange(listingQtr, GISJOIN) %>%
  mutate(p1B = n1B/nListings)


#### ACF for panel ------------------------------------------------------------

acf_df <- data.frame(sea_df = unique(sea_df$GISJOIN), acf = rep(NA, length(unique(sea_df$GISJOIN))))
for(i in unique(sea_df$GISJOIN)){
  obs <- sea_df[sea_df$GISJOIN == i,]
  ts <- ts(obs$medRent)
  if(sum(is.na(ts)) > 0){
    next
  } else{
    #print(ts)
    acf <- acf(ts)
    #print(acf)
    acf_df$acf[acf_df$sea_df == tract] = list(acf)
  }
}

#ACFs are weak with only 5 periods to assess with. Highest L1 value is around .5 but many are
#weakened by a noisey quarter


#### B. INLA Models of CL Rent ------------------------------------------------

library(INLA)

#geo information for INLA
sea_shp <- readOGR(dsn = "R:/Project/seattle_rental_market/data/geo/sea_tract_2010/sea_tract_2010.shp",
                   layer = "sea_tract_2010",
                   GDAL1_integer64_policy = TRUE,
                   stringsAsFactors = F)

#compute centroid for map of tract numbers
cens <- gCentroid(sea_shp, byid = T)
cens <- data.frame(long = cens@coords[,1],
                   lat = cens@coords[,2])
cens <- cens %>% mutate(row = row_number())
sea_ff <- fortify(sea_shp)

#plot seattle tracts with tract number labels
ggplot(sea_ff, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "grey90", color = "white") +
  geom_text(data = cens, aes(x  = long, y = lat, label = row, group = row))

#create adjacency matrix from shapefile
sea_adj <- poly2nb(sea_shp)

#create neighbor file that INLA takes for hyperparameter args
nb2INLA("R:/Project/seattle_rental_market/report/spatial_epi/seatract.graph", sea_adj)
sea_df$idqtr <- factor(sea_df$listingQtr, ordered = T)
sea_df$idqtr1 <- sea_df$idqtr
sea_df$idtractqtr <- paste(sea_df$idtract, sea_df$idqtr)
sea_df$idtract1 <- sea_df$idtract

#fixed effect for qtr only
form.int <- log(med1B) ~ 1 + Qtr 

m.int <- inla(form.int, 
              family = "normal", 
              data = sea_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.int)
sea_df$int_Med <- m.int$summary.linear.predictor[, "0.5quant"]
sea_df$int_SD <- m.int$summary.linear.predictor[, "sd"]
sea_df$int_postWidth <- m.int$summary.linear.predictor[, "0.975quant"] - m.int$summary.linear.predictor[,"0.025quant"]
sea_df$int_Eff <- m.int$summary.random$idtract[sea_df$idtract, "0.5quant"] 

#fixed qtr + iid tract random effect
form.ns <- log(med1B) ~ 1 + Qtr +
  f(idtract, model = "iid")

m.ns <- inla(form.ns, 
             family = "normal", 
             data = sea_df,
             control.predictor = list(compute = TRUE),
             control.compute = list(dic = TRUE, waic = TRUE))

summary(m.ns)
sea_df$ns_Med <- m.ns$summary.linear.predictor[, "0.5quant"]
sea_df$ns_SD <- m.ns$summary.linear.predictor[, "sd"]
sea_df$ns_postWidth <- m.ns$summary.linear.predictor[, "0.975quant"] - m.ns$summary.linear.predictor[,"0.025quant"]
sea_df$ns_Eff <- m.ns$summary.random$idtract[sea_df$idtract, "0.5quant"] 

#IID tract random effect + AR(1) process
form.nsar1 <- log(med1B) ~ 1 +
  f(idtract, model = "iid") +
  f(idqtr, model = "ar1") + f(idqtr1, model = "iid")

m.nsar1 <- inla(form.nsar1, 
                family = "normal", 
                data = sea_df,
                control.predictor = list(compute = TRUE),
                control.compute = list(dic = TRUE, waic = TRUE))

summary(m.nsar1) #NB: does not improve fit to use random effect time specification, using fixed effects
sea_df$nsar1_Med <- m.nsar1$summary.linear.predictor[, "0.5quant"]
sea_df$nsar1_SD <- m.nsar1$summary.linear.predictor[, "sd"]
sea_df$nsar1_postWidth <- m.nsar1$summary.linear.predictor[, "0.975quant"] - m.nsar1$summary.linear.predictor[,"0.025quant"]
sea_df$nsar1_Eff <- m.nsar1$summary.random$idtract[sea_df$idtract, "0.5quant"] 

#spatial random effect model (BYM 1991) with seasonal dummies
form.bym <- log(med1B) ~ 1 + Qtr +
  f(idtract, model = "bym", #ICAR spatial RE for tract neighbors + IID RE for tract
    scale.model = T,
    graph = "R:/Project/seattle_rental_market/report/spatial_epi/seatract.graph")

m.bym <- inla(form.bym, 
              family = "normal", 
              data = sea_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.bym)
sea_df$bym_Med <- m.bym$summary.linear.predictor[, "0.5quant"]
sea_df$bym_SD <- m.bym$summary.linear.predictor[, "sd"]
sea_df$bym_postWidth <- m.bym$summary.linear.predictor[, "0.975quant"] - m.bym$summary.linear.predictor[,"0.025quant"]
sea_df$bym_Eff <- m.bym$summary.random$idtract[sea_df$idtract, "0.5quant"] 

#spatio-temporal smoothing model with linear space-time interaction (Bernadelli 1995)
form.spt <- log(med1B) ~ 1 + Qtr +
  f(idtract, model = "bym", #ICAR spatial RE for tract neighbors + IID RE for tract
    scale.model = T,
    graph = "R:/Project/seattle_rental_market/report/spatial_epi/seatract.graph") +
  f(idtractqtr, model="iid") + #exchangeable RE for period
  f(idqtr, model = "iid") 

m.spt <- inla(form.spt, 
              family = "normal", 
              data = sea_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.spt)
sea_df$spt_Med <- m.spt$summary.linear.predictor[, "0.5quant"]
sea_df$spt_SD <- m.spt$summary.linear.predictor[, "sd"]
sea_df$spt_postWidth <- m.spt$summary.linear.predictor[, "0.975quant"] - m.spt$summary.linear.predictor[,"0.025quant"]
sea_df$spt_Eff <- m.spt$summary.random$idtract[sea_df$idtract, "0.5quant"] 

#overfitting? too much bumpiness qtr-to-qtr

#### Maps of Model Output ------------------------------------------------------------------

sea_df <- sea_df %>%
  mutate_at(.vars = vars(matches("_Med")),
            .funs = exp) %>%
  mutate_at(.vars = vars(matches("_SD")),
            .funs = exp) %>%
  group_by(GISJOIN) %>% 
  arrange(GISJOIN, listingQtr) %>%
  mutate(spt_traj = spt_Med - lag(spt_Med, 4)) %>%
  ungroup %>%
  group_by(GISJOIN) %>%
  mutate(spt_traj = sum(spt_traj, na.rm=T))

sea_df

sea_shp@data$id <- rownames(sea_shp@data)
sea_shp@data <- left_join(sea_shp@data, sea_df)
sea_f <- fortify(sea_shp)
sea_f <- inner_join(sea_f, sea_shp@data,"id")

rmse <- function(error){sqrt(mean(error^2))}
mae <- function(error){mean(abs(error))}

#table of fit statistics
fit_stats <- sea_df %>%
  mutate(train_test = ifelse(listingQtr == "2018 Q2", "Test", "Training")) %>%
  mutate(int_err = actualRent - int_Med,
         ns_err = actualRent - ns_Med,
         nsar1_err = actualRent - nsar1_Med,
         bym_err = actualRent - bym_Med,
         spt_err = actualRent - spt_Med) %>% 
  group_by(train_test) %>%
  summarize(int_rmse = rmse(int_err[!is.na(int_err)]),
            int_mae = mae(int_err[!is.na(int_err)]),
            ns_rmse = rmse(ns_err[!is.na(ns_err)]),
            ns_mae = mae(ns_err[!is.na(ns_err)]),
            nsar1_rmse = rmse(nsar1_err[!is.na(nsar1_err)]),
            nsar1_mae = mae(nsar1_err[!is.na(nsar1_err)]),
            bym_rmse = rmse(bym_err[!is.na(bym_err)]),
            bym_mae = mae(bym_err[!is.na(bym_err)]),
            spt_rmse = rmse(spt_err[!is.na(spt_err)]),
            spt_mae = mae(spt_err[!is.na(spt_err)])) %>%
  dplyr::select(train_test, ends_with("_rmse"), ends_with("_mae"))

#aspatial graphics

#scatterplots
ggplot(sea_df, aes(x = actualRent, y = int_Med)) +
  facet_grid(. ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Fixed Intercept Only model$",
       subtitle = TeX("$\\hat{y}_{i, t, b} = \\alpha_{b}$")) +
  ggsave(filename = "./output/avp_fixedint.pdf",
         width = 10, height = 8)

ggplot(sea_df, aes(x = actualRent, y = ns_Med)) +
  facet_grid(. ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian()  +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Non-Spatial RE model with seasonal dummy",
       subtitle = TeX("$\\hat{y}_{i, t} = \\alpha_{b} + \\epsilon_{i}$")) +
  ggsave(filename = "./output/avp_nsre.pdf",
         width = 10, height = 8)

ggplot(sea_df, aes(x = actualRent, y = nsar1_Med)) +
  facet_grid(. ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Non-Spatial RE, AR(1) model",
       subtitle = TeX("$\\hat{y}_{i, t} = \\epsilon_{i} + \\epsilon_{t} + \\zeta_{t-1}$")) +
  ggsave(filename = "./output/avp_nsar1.pdf",
         width = 10, height = 8)

ggplot(sea_df, aes(x = actualRent, y = bym_Med)) +
  facet_grid( ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for ICAR Spatial RE model with Seasonal dummies",
       subtitle = TeX("$\\hat{y}_{i, t} = \\Beta_{1}*qtr + \\epsilon_{i}$ + S_i")) +
  ggsave(filename = "./output/avp_bym.pdf",
         width = 10, height = 8)

ggplot(sea_df, aes(x = actualRent, y = spt_Med)) +
  facet_grid( ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Spatiotemporal RE model with seasonal dummies",
       subtitle = TeX("$\\hat{y}_{i, t} = \\Beta_{1}*qtr + \\epsilon_{tract}$")) +
  ggsave(filename = "./output/avp_spt.pdf",
         width = 10, height = 8)

#lineplot
ggplot(sea_df, 
       aes(x = listingQtr, y = bym_Med, group = GISJOIN, color = listingQtr == "2018 Q2")) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("\nQuarter") +
  ylab("Median Prediction\n") +
  labs(title = "ICAR Spatial RE model lineplot", color = "Forecast?") +
  ggsave(filename = "./output/bedsLineplot_bym.pdf",
         width = 10, height = 10, dpi = 300)

#lineplot
ggplot(sea_df, 
       aes(x = listingQtr, y = spt_Med, group = GISJOIN, color = listingQtr == "2018 Q2")) +
  facet_wrap(~ spt_traj > 0) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("\nQuarter") +
  ylab("Median Prediction\n") +
  labs(title = "Spatiotemporal RE model lineplot", color = "Forecast?") +
  ggsave(filename = "./output/bedsLineplot_spt.pdf",
         width = 10, height = 10, dpi = 300)

#maps
library(gridExtra)

#observed and smoothed rent during test periods
obs <- ggplot(sea_f %>% filter(listingQtr == "2018 Q2"), 
       aes(x = long, y = lat, group = group, fill = actualRent)) +
  geom_polygon(color = "grey85", lwd = .05) +
  scale_fill_viridis_c(limits = c(750, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Observed Median Rent",
       fill = "Observed")

smoothed <- ggplot(sea_f %>% filter(listingQtr == "2018 Q2"), 
              aes(x = long, y = lat, group = group, fill = bym_Med)) +
  geom_polygon(color = "grey85", lwd = .05) +
  scale_fill_viridis_c(limits = c(750, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Spatially Smoothed Median Rent",
       fill = "Smoothed")

combined <- grid.arrange(obs, smoothed, nrow = 1)

ggsave(combined,
       filename = "./output/map_avp_bym_1B.pdf",
       width = 8, height = 6, dpi = 300)

#observed and smoothed rent during test periods
s_smoothed <- ggplot(sea_f %>% filter(listingQtr == "2018 Q2"), 
                   aes(x = long, y = lat, group = group, fill = spt_Med)) +
  geom_polygon(color = "grey85", lwd = .05) +
  scale_fill_viridis_c(limits = c(750, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "ST Smoothed One-Bedroom Median Rent",
       fill = "Smoothed")

combined_test <- grid.arrange(obs, s_smoothed, nrow = 1)

ggsave(combined_test,
       filename = "./output/map_avp_spt_1B.pdf",
       width = 8, height = 6, dpi = 300)

# actual vs predicted difference
smoothed <- ggplot(sea_f, 
                   aes(x = long, y = lat, group = group, fill = bym_Med)) +
  facet_grid(~ listingQtr) +
  geom_polygon(color = "grey85", lwd = .05) +
  scale_fill_viridis_c(limits = c(750, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Spatially Smoothed 1B Median Rent",
       fill = "Smoothed")

st_smoothed <- ggplot(sea_f, 
                      aes(x = long, y = lat, group = group, fill = spt_Med)) +
  facet_grid(~ listingQtr) +
  geom_polygon(color = "grey85", lwd = .05) +
  scale_fill_viridis_c(limits = c(750, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Spatiotemporal Smoothed 1B Median Rent",
       fill = "Smoothed")

combined_train <- grid.arrange(smoothed, st_smoothed, nrow = 2)

ggsave(combined_train,
       filename = "./output/map_bym_spt_1B.pdf",
       width = 11, height = 8, dpi = 300)


