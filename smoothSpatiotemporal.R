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
library(rgeos)
library(sqldf)
library(spdep)
library(viridis)
library(latex2exp)

#setwd to location of file (REQUIRES RSTUDIO)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load ACS data extract (codebook in folder)
census <- read_csv(file = "./input/acsExtract.csv")

#read in tract shapefiles for king county
kc_shp <- readOGR(dsn = "R:/Project/seattle_rental_market/data/geo/KCTract2010/KCTract2010.shp",
                  layer = "KCTract2010",
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
    filter(!is.na(GISJOIN), !is.na(cleanBeds), !is.na(cleanRent), !is.na(cleanSqft)) %>% #only listings with valid Bed/Rent
    filter(GISJOIN %in% kc_shp@data$GISJOIN) %>% #filter to KC only (db has metro area)
    distinct(cleanBeds, cleanRent, cleanSqft, matchAddress, matchAddress2, .keep_all = T) %>% #dedupe to unique address-bed-rent combos
    dplyr::select(listingDate, GEOID10, GISJOIN, seattle, matchAddress, matchType, cleanBeds, cleanRent, cleanSqft) %>% #SELECT these columns
    mutate(listingDate = as.Date(listingDate),
           listingQtr = as.yearqtr(listingDate)) %>%
    filter(listingDate >= "2017-04-01", cleanBeds %in% c(0, 1, 2, 3)) %>%
    group_by(GEOID10, GISJOIN, listingQtr) %>% #group listings by tract, qtr within tract
    summarize(nListings = n(),
              nGT2B = sum(cleanBeds > 2),
              medRent = median(cleanRent),
              med0B = median(cleanRent[cleanBeds==0]),
              med1B = median(cleanRent[cleanBeds==1]),
              med2B = median(cleanRent[cleanBeds==2]),
              med3B = median(cleanRent[cleanBeds==3]),
              lagRent = lag(cleanRent),
              seattle = max(seattle)) %>% #create tract aggregates
    mutate(pGT2B = nGT2B/nListings) %>%
    ungroup %>%
    arrange(GISJOIN, listingQtr)
  dbDisconnect(DB)
  write_csv(tract, "./input/tractCl.csv")
  
} else{
  #if github config, read in extract from cl db
  tract <- read_csv("./input/tractCl.csv")
}


#### A. construct panel (some missingness) ------------------------------------

census <- census[match(kc_shp@data$GISJOIN, census$GISJOIN),]

census <- census %>%
  filter(STATEA == "53", COUNTYA == "033") %>%
  mutate(idtract1 = idtract,
         nHU = AF7PE001,
         pOwnoccHU = AF7PE002/AF7PE001,
         medHUVal = AF9LE001)

### Training periods

#studios
Q2_2017 <- tract %>%
  filter(listingQtr == "2017 Q2") %>%
  right_join(census) %>%
  ungroup %>%
  mutate(listingQtr = "2017 Q2",
         Qtr = "Q2",
         actualRent = medRent)

Q3_2017 <- tract %>%
  filter(listingQtr == "2017 Q3") %>%
  right_join(census) %>%
  ungroup %>%
  mutate(listingQtr = "2017 Q3",
         Qtr = "Q3",
         actualRent = medRent)

Q4_2017 <- tract %>%
  filter(listingQtr == "2017 Q4") %>%
  right_join(census) %>%
  ungroup %>%
  mutate(listingQtr = "2017 Q4",
         Qtr = "Q4",
         actualRent = medRent)

Q1_2018 <- tract %>%
  filter(listingQtr == "2018 Q1") %>%
  right_join(census) %>%
  ungroup %>%
  mutate(listingQtr = "2018 Q1",
         Qtr = "Q1",
         actualRent = medRent)

### Forecast/test period

#studio
Q2_2018 <- tract %>%
  filter(listingQtr == "2018 Q2") %>%
  right_join(census) %>%
  ungroup %>%
  mutate(listingQtr = "2018 Q2",
         Qtr = "Q2",
         actualRent = medRent,
         medRent = NA)

kc_df <- bind_rows(Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018)
kc_df$listingQtr <- as.yearqtr(kc_df$listingQtr)

kc_df <- kc_df %>% 
  dplyr::select(-GEOID10) %>%
  dplyr::select(GISJOIN, listingQtr, Qtr, idtract, idtract1, medRent, lagRent, actualRent, nHU, 
                pOwnoccHU, medHUVal, pGT2B)


#### ACF for panel ------------------------------------------------------------

acf_df <- data.frame(kc_df = unique(kc_df$GISJOIN), acf = rep(NA, length(unique(kc_df$GISJOIN))))
for(i in unique(kc_df$GISJOIN)){
  obs <- kc_df[kc_df$GISJOIN == i,]
  ts <- ts(obs$medRent)
  if(sum(is.na(ts)) > 0){
    next
  } else{
    #print(ts)
    acf <- acf(ts)
    #print(acf)
    acf_df$acf[acf_df$kc_df == tract] = list(acf)
  }
}

#ACFs are weak with only 5 periods to assess with. Highest L1 value is around .5 but many are
#weakened by a noisey quarter


#### F. INLA Models of CL Rent ------------------------------------------------

library(INLA)

#geo information for INLA
kc_shp <- readOGR(dsn = "R:/Project/seattle_rental_market/data/geo/KCTract2010/KCTract2010.shp",
                  layer = "KCTract2010",
                  GDAL1_integer64_policy = TRUE,
                  stringsAsFactors = F)

cens <- gCentroid(kc_shp, byid = T)
cens <- data.frame(long = cens@coords[,1],
                   lat = cens@coords[,2])
cens <- cens %>% mutate(row = row_number())
kc_ff <- fortify(kc_shp)

ggplot(kc_ff, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "grey90", color = "white") +
  geom_text(data = cens, aes(x  = long, y = lat, label = row, group = row))

kc_adj <- poly2nb(kc_shp)
nb2INLA("R:/Project/seattle_rental_market/report/spatial_epi/kctract.graph", kc_adj)
kc_df$idqtr <- as.character(kc_df$listingQtr)
kc_df$idqtr1 <- kc_df$idqtr
kc_df$idtractqtr <- paste(kc_df$idtract, kc_df$idqtr)

kc_df <- kc_df %>%
  arrange(GISJOIN, listingQtr)

#King County median rent models -----------------------------------------------

#random intercept for qtr only
form.int <- medRent ~ -1 + f(idqtr1, model = "iid")

m.int <- inla(form.int, 
             family = "lognormal", 
             data = kc_df,
             control.predictor = list(compute = TRUE),
             control.compute = list(dic = TRUE, waic = TRUE))

summary(m.int)
kc_df$int_Med <- m.int$summary.linear.predictor[, "0.5quant"]
kc_df$int_SD <- m.int$summary.linear.predictor[, "sd"]
kc_df$int_postWidth <- m.int$summary.linear.predictor[, "0.975quant"] - m.int$summary.linear.predictor[,"0.025quant"]
kc_df$int_Eff <- m.int$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#fixed intercept for beds + iid random effect
form.ns <- medRent ~ 1 + Qtr + f(idtract, model = "iid")

m.ns <- inla(form.ns, 
              family = "normal", 
              data = kc_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.ns)
kc_df$ns_Med <- m.ns$summary.linear.predictor[, "0.5quant"]
kc_df$ns_SD <- m.ns$summary.linear.predictor[, "sd"]
kc_df$ns_postWidth <- m.ns$summary.linear.predictor[, "0.975quant"] - m.ns$summary.linear.predictor[,"0.025quant"]
kc_df$ns_Eff <- m.ns$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#fixed intercept for beds + fixed effects for quarter + tract IID random effect
form.nst <- medRent ~ 1 + Qtr + f(idtract, model = "iid")

m.nst <- inla(form.nst, 
              family = "lognormal", 
              data = kc_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.nst)
kc_df$nst_Med <- m.nst$summary.linear.predictor[, "0.5quant"]
kc_df$nst_SD <- m.nst$summary.linear.predictor[, "sd"]
kc_df$nst_postWidth <- m.nst$summary.linear.predictor[, "0.975quant"] - m.nst$summary.linear.predictor[,"0.025quant"]
kc_df$nst_Eff <- m.nst$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#fixed intercept for beds + IID tract random effect + AR(1) process
form.nsar1 <- medRent ~ 1 + f(idtract, model = "iid") +
  f(idqtr, model = "ar1") + f(idqtr1, model = "iid")

m.nsar1 <- inla(form.nsar1, 
               family = "normal", 
               data = kc_df,
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE))

summary(m.nsar1) #NB: does not improve fit to use random effect time specification, using fixed effects
kc_df$nsar1_Med <- m.nsar1$summary.linear.predictor[, "0.5quant"]
kc_df$nsar1_SD <- m.nsar1$summary.linear.predictor[, "sd"]
kc_df$nsar1_postWidth <- m.nsar1$summary.linear.predictor[, "0.975quant"] - m.nsar1$summary.linear.predictor[,"0.025quant"]
kc_df$nsar1_Eff <- m.nsar1$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#spatial random effect model (BYM 1991) with seasonal dummies
form.bym <- medRent ~ 1 + Qtr +
  f(idtract, model = "bym", #ICAR spatial RE for tract neighbors + IID RE for tract
    graph = "R:/Project/seattle_rental_market/report/spatial_epi/kctract.graph")

m.bym <- inla(form.bym, 
             family = "normal", 
             data = kc_df,
             control.predictor = list(compute = TRUE),
             control.compute = list(dic = TRUE, waic = TRUE))

summary(m.bym)
kc_df$bym_Med <- m.bym$summary.linear.predictor[, "0.5quant"]
kc_df$bym_SD <- m.bym$summary.linear.predictor[, "sd"]
kc_df$bym_postWidth <- m.bym$summary.linear.predictor[, "0.975quant"] - m.bym$summary.linear.predictor[,"0.025quant"]
kc_df$bym_Eff <- m.bym$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#spatio-temporal smoothing model with linear space-time interaction (Bernadelli 1995)
form.spt <- medRent ~ 1 + idqtr +
  f(idtract, model = "bym", #ICAR spatial RE for tract neighbors + IID RE for tract
    graph = "R:/Project/seattle_rental_market/report/spatial_epi/kctract.graph") +
  f(idtractqtr, model="iid") #exchangeable RE for period

m.spt <- inla(form.spt, 
              family = "normal", 
              data = kc_df,
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE))

summary(m.spt)
kc_df$spt_Med <- m.spt$summary.linear.predictor[, "0.5quant"]
kc_df$spt_SD <- m.spt$summary.linear.predictor[, "sd"]
kc_df$spt_postWidth <- m.spt$summary.linear.predictor[, "0.975quant"] - m.spt$summary.linear.predictor[,"0.025quant"]
kc_df$spt_Eff <- m.spt$summary.random$idtract[kc_df$idtract, "0.5quant"] 

#does not improve ICs much, but might still perform better for forecasting

#### Maps of Model Output ------------------------------------------------------------------

#kc_df <- kc_df %>%
#  mutate_at(.vars = vars(matches("_Med")),
#            .funs = exp) %>%
#  mutate_at(.vars = vars(matches("_SD")),
#            .funs = exp) 

kc_shp@data$id <- rownames(kc_shp@data)
kc_shp@data <- left_join(kc_shp@data, kc_df)
kc_f <- fortify(kc_shp)
kc_f <- inner_join(kc_f, kc_shp@data,"id")

rmse <- function(error){sqrt(mean(error^2))}
mae <- function(error){mean(abs(error))}

#table of fit statistics
fit_stats <- kc_df %>%
  mutate(train_test = ifelse(listingQtr == "2018 Q2", "Test", "Training")) %>%
  mutate(int_err = actualRent - int_Med,
         ns_err = actualRent - ns_Med,
         nst_err = actualRent - nst_Med,
         nsar1_err = actualRent - nsar1_Med,
         bym_err = actualRent - bym_Med,
         spt_err = actualRent - spt_Med) %>% 
  group_by(train_test) %>%
  summarize(int_rmse = rmse(int_err[!is.na(int_err)]),
            int_mae = mae(int_err[!is.na(int_err)]),
            ns_rmse = rmse(ns_err[!is.na(ns_err)]),
            ns_mae = mae(ns_err[!is.na(ns_err)]),
            nst_rmse = rmse(nst_err[!is.na(nst_err)]),
            nst_mae = mae(nst_err[!is.na(nst_err)]),
            nsar1_rmse = rmse(nsar1_err[!is.na(nsar1_err)]),
            nsar1_mae = mae(nsar1_err[!is.na(nsar1_err)]),
            bym_rmse = rmse(bym_err[!is.na(bym_err)]),
            bym_mae = mae(bym_err[!is.na(bym_err)]),
            spt_rmse = rmse(spt_err[!is.na(spt_err)]),
            spt_mae = mae(spt_err[!is.na(spt_err)])) %>%
  dplyr::select(train_test, ends_with("_rmse"), ends_with("_mae"))

#aspatial graphics

#scatterplots
ggplot(kc_df, aes(x = actualRent, y = int_Med)) +
  facet_grid(beds ~ listingQtr) +
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

ggplot(kc_df, aes(x = actualRent, y = ns_Med)) +
  facet_grid(beds ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian()  +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Non-Spatial RE model",
       subtitle = TeX("$\\hat{y}_{i, t, b} = \\alpha_{b} + \\epsilon_{i}$")) +
  ggsave(filename = "./output/avp_nsre.pdf",
         width = 10, height = 8)

ggplot(kc_df, aes(x = actualRent, y = nst_Med)) +
  facet_grid(beds ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Non-Spatial RE with Seasonal dummy model",
       subtitle = TeX("$\\hat{y}_{i, t, b} = \\alpha_{b} + \\Beta_{1}*Qtr + \\epsilon_{i}$")) +
  ggsave(filename = "./output/avp_nsre.pdf",
         width = 10, height = 8)

ggplot(kc_df, aes(x = actualRent, y = nsar1_Med)) +
  facet_grid(beds ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for Non-Spatial RE, AR(1) model",
       subtitle = TeX("$\\hat{y}_{t, beds, tract} = \\alpha_{b} + \\epsilon_{i} + \\epsilon_{t} + \\zeta_{t-1}$")) +
  ggsave(filename = "./output/avp_nsar1.pdf",
         width = 10, height = 8)

ggplot(kc_df, aes(x = actualRent, y = bym_Med)) +
  facet_grid(beds ~ listingQtr) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian() +
  xlab("\nObserved Rent") +
  ylab("Predicted Rent\n") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Actual v. Predicted for ICAR Spatial RE with Seasonal dummy model",
       subtitle = TeX("$\\hat{y}_{qtr, beds, tract} = \\alpha_{beds} + \\Beta_{1}*qtr + \\epsilon_{tract}$")) +
  ggsave(filename = "./output/avp_nsre.pdf",
         width = 10, height = 8)

#lineplot
ggplot(kc_df, 
       aes(x = listingQtr, y = exp(bym_Med), group = GISJOIN, color = listingQtr == "2018 Q2")) +
  facet_grid(~ beds) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(color = "Forecast?") +
  ggsave(filename = "./output/bedsLineplot.pdf",
         width = 10, height = 10, dpi = 300)

#maps
library(gridExtra)

#observed and smoothed rent during test periods
obs <- ggplot(kc_f %>% filter(beds == "1 Bedroom", listingQtr == "2018 Q2"), 
       aes(x = long, y = lat, group = group, fill = actualRent)) +
  geom_polygon() +
  #facet_grid(~ listingQtr) +
  scale_fill_viridis_c(limits = c(900, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Observed One-Bedroom Median Rent",
       fill = "Observed")

smoothed <- ggplot(kc_f %>% filter(beds == "1 Bedroom", listingQtr == "2018 Q2"), 
              aes(x = long, y = lat, group = group, fill = ns_Med)) +
  geom_polygon() +
  #facet_grid(~ listingQtr) +
  scale_fill_viridis_c(limits = c(900, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Smoothed One-Bedroom Median Rent",
       fill = "Smoothed")

combined <- grid.arrange(obs, smoothed, nrow = 1)

ggsave(combined,
       filename = "./output/map_avp_ns_1B.pdf",
       width = 12, height = 5, dpi = 300)

#observed and smoothed rent during test periods
smoothed <- ggplot(kc_f %>% filter(beds == "1 Bedroom", listingQtr == "2018 Q2"), 
                   aes(x = long, y = lat, group = group, fill = bym_Med)) +
  geom_polygon() +
  #facet_grid(~ listingQtr) +
  scale_fill_viridis_c(limits = c(900, 2500), labels = scales::dollar) +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Smoothed One-Bedroom Median Rent",
       fill = "Smoothed")

combined <- grid.arrange(obs, smoothed, nrow = 1)

ggsave(combined,
       filename = "./output/map_avp_bym_1B.pdf",
       width = 12, height = 5, dpi = 300)

# actual vs predicted difference
ggplot(kc_f %>% filter(listingQtr != "2018 Q2"), 
       aes(x = long, y = lat, group = group, fill = medRent - bym_Med)) +
  geom_polygon() +
  facet_grid(beds ~ listingQtr) +
  scale_fill_gradient2() +
  coord_quickmap() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())








