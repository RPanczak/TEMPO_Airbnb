# ############################################
# title: "Airbnb - income in 3D"
# subtitle: "Data prep for ArcGIS Pro"
# ############################################

set.seed(12345)
options(scipen = 999)

library(pacman) 
p_load(tidyverse, magrittr, readxl, janitor, anytime, lubridate, scales, 
       sf, tmap, tmaptools, plotKML, 
       skimr, summarytools, sjmisc, kableExtra, naniar)

tmap_mode("view") # makes map interactive


# ############################################
# centroids
SA2_centr <- st_read("./data/geo/1270055001_sa2_2016_aust_shape/SA2_2016_AUST_centr_inside.shp", stringsAsFactors = FALSE) %>% 
  filter(SA4_CODE16 != "901") %>% 
  select(-starts_with("SA3")) %>% 
  select(-starts_with("SA4")) %>% 
  select(-starts_with("GCC")) %>%
  select(-AREASQKM16) %>% 
  mutate(STE_CODE16 = as.numeric(as.character(STE_CODE16))) %>% 
  mutate(SA2_MAIN16 = as.numeric(as.character(SA2_MAIN16))) %>% 
  mutate(SA2_5DIG16 = as.numeric(as.character(SA2_5DIG16))) %>% 
  select(SA2_MAIN16, POINT_X, POINT_Y) %>% 
  rename(lat = POINT_X, lon = POINT_Y) %>% 
  st_drop_geometry()

# ############################################
# 38 mo
airbnb_sa2 <- readRDS(file = "./data/airdna/clean/airbnb_sa2.rds") %>% 
  select(-IRSD_d, -IRSAD_d, -IER_d, -IEO_d, -coast_bin) %>% 
  # filter(STE_CODE16 == 3) %>% 
  rename(start = reporting_month) %>% 
  select(-starts_with("STE"))

airbnb_sa2$end <- ceiling_date(airbnb_sa2$start, "month") - days(1)

length(unique(SA2_centr$SA2_MAIN16))
length(unique(airbnb_sa2$SA2_MAIN16))

airbnb_sa2 %>% 
  left_join(SA2_centr) %>% 
  write_csv("./gis/ST_SA2/raw/airbnb_sa2_38m_abs.csv")
  # st_transform(3112) %>% 
  # st_write("./gis/ST_SA2_16mo_absolute/raw/airbnb_sa2_38m_abs.shp")#, delete_layer = TRUE) # overwrites


# ############################################
# 12 mo
airbnb_sa2 <- readRDS(file = "./data/airdna/clean/airbnb_sa2.rds")
  
airbnb_sa2_12mo <- airbnb_sa2 %>% 
  select(-IRSD_d, -IRSAD_d, -IER_d, -IEO_d, -coast_bin) %>% 
  filter(reporting_month >= anydate("2018-01-01")) %>% 
  filter(reporting_month <= anydate("2018-12-01")) %>% 
  rename(start = reporting_month) %>% 
  select(-starts_with("STE")) %>% 
  group_by(SA2_MAIN16) %>% 
  mutate(revenue_total = sum(revenue)) %>% 
  ungroup() %>% 
  mutate(revenue_rel = (revenue/revenue_total)*100) %>% 
  mutate(revenue_rel = ifelse(revenue==0 & revenue_total==0, 0, revenue_rel)) %>% 
  arrange(SA2_MAIN16, start) 

airbnb_sa2_12mo$end <- ceiling_date(airbnb_sa2_12mo$start, "month") - days(1)

length(unique(SA2_centr$SA2_MAIN16))
length(unique(airbnb_sa2_12mo$SA2_MAIN16))

airbnb_sa2_12mo %>% 
  left_join(SA2_centr) %>% 
  write_csv("./gis/ST_SA2/raw/airbnb_sa2_12mo_rel.csv")
# st_transform(3112) %>% 
# st_write("./gis/ST_SA2_16mo_absolute/raw/airbnb_sa2_12mo.shp")#, delete_layer = TRUE) # overwrites

# ############################################
# point data
# airbnb_monthly <- readRDS(file = "./data/airdna/clean/airbnb_monthly.rds") %>% 
#   filter(STE_CODE16 == 3)
# 
# airbnb_monthly$reporting_month_end <- ceiling_date(airbnb_monthly$reporting_month, "month") - days(1)




