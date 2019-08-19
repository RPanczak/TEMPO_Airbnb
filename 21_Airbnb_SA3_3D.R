# ############################################
# title: "Airbnb - income in 3D"
# subtitle: "Data prep for ArcGIS Pro"
# change to reservations & SA3
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
SA3_centr <- readRDS(file = "./data/geo/SA3_centr.rds") %>% 
  mutate(SA3_CODE16	= as.numeric(SA3_CODE16)) %>% 
  filter(SA3_CODE16	!= 10702) %>% 
  st_drop_geometry()

# ############################################
# 38 mo
airbnb_sa3 <- readRDS(file = "./data/airdna/clean/airbnb_sa3.rds") %>% 
  rename(start = reporting_month) %>% 
  select(-starts_with("STE"))

airbnb_sa3$end <- ceiling_date(airbnb_sa3$start, "month") - days(1)

length(unique(SA3_centr$SA3_CODE16)) == length(unique(airbnb_sa3$SA3_CODE16))

airbnb_sa3 %>% 
  left_join(SA3_centr) %>% 
  write_csv("./gis/ST_SA3/raw/airbnb_sa3_38m_abs.csv")
  # st_transform(3112) %>% 
  # st_write("./gis/ST_SA2_16mo_absolute/raw/airbnb_sa2_38m_abs.shp")#, delete_layer = TRUE) # overwrites


# ############################################
# 12 mo
airbnb_sa3_12mo <- readRDS(file = "./data/airdna/clean/airbnb_sa3_18.rds") %>% 
  select(-starts_with("STE")) %>% 
  rename(start = reporting_month) 

airbnb_sa3_12mo$end <- ceiling_date(airbnb_sa3_12mo$start, "month") - days(1)

length(unique(SA3_centr$SA3_CODE16)) == length(unique(airbnb_sa3_12mo$SA3_CODE16))

airbnb_sa3_12mo %>% 
  left_join(SA3_centr) %>% 
  write_csv("./gis/ST_SA3/raw/airbnb_sa3_12mo_rel.csv")
# st_transform(3112) %>% 
# st_write("./gis/ST_SA2_16mo_absolute/raw/airbnb_sa3_12mo.shp")#, delete_layer = TRUE) # overwrites

# ############################################
# point data
# airbnb_monthly <- readRDS(file = "./data/airdna/clean/airbnb_monthly.rds") %>% 
#   filter(STE_CODE16 == 3)
# 
# airbnb_monthly$reporting_month_end <- ceiling_date(airbnb_monthly$reporting_month, "month") - days(1)




