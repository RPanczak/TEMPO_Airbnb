# ########################################
# Testing glm on Airbnb data
# SA1 level input
# no time - aggregated 11mo of data used
# state as RE
# various specs of adjustment
# ########################################

set.seed(12345)
options(scipen = 999)

# ########################################
library(glmmTMB)
library(bbmle)
library(DHARMa)
library(magrittr)
library(dplyr)
library(ggplot2)
library(sjPlot)

# https://cran.r-project.org/web/packages/glmmTMB/index.html

# ########################################
# ########################################
# ########################################
# data
airbnb_sa1 <- readRDS(file = "./data/airdna/clean/airbnb_sa1.rds") %>% 
  dplyr::filter(reporting_month >= as.Date("2016-03-01") & reporting_month <= as.Date("2017-01-01")) %>% 
  dplyr::group_by(SA1_MAIN16) %>% 
  dplyr::mutate(
    revenue = sum(revenue),
    IRSD_d = first(IRSD_d),
    IRSAD_d = first(IRSAD_d), 
    IER_d = first(IER_d),
    IEO_d = first(IEO_d),
    STE_NAME16 = first(STE_NAME16),
    SOS_NAME_2016 = first(SOS_NAME_2016),
    RA_NAME_2016 = first(RA_NAME_2016),
    coast_bin = first(coast_bin)
  ) %>% 
  dplyr::filter(row_number()==1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    IRSD_d = factor(IRSD_d),
    IRSAD_d = factor(IRSAD_d), 
    IER_d = factor(IER_d),
    IEO_d = factor(IEO_d),
    STE_NAME16 = factor(STE_NAME16),
    SOS_NAME_2016 = factor(SOS_NAME_2016),
    RA_NAME_2016 = factor(RA_NAME_2016),
    coast_bin = factor(coast_bin, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>% 
  dplyr::select(-reporting_month, -cumulative) 

# ref categories for factors
# table(airbnb_sa1$STE_NAME16)  
airbnb_sa1$STE_NAME16 <- relevel(airbnb_sa1$STE_NAME16, ref = "New South Wales")

# table(airbnb_sa1$SOS_NAME_2016)  
airbnb_sa1$SOS_NAME_2016 <- relevel(airbnb_sa1$SOS_NAME_2016, ref = "Major Urban")

# table(airbnb_sa1$RA_NAME_2016)  
airbnb_sa1$RA_NAME_2016 <- relevel(airbnb_sa1$RA_NAME_2016, ref = "Major Cities of Australia")

# ########################################
# ########################################
# ########################################
# IRSD_d
m10 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ 0,
               family = nbinom1)

m11 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d,
               family = nbinom1)

m12 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + RA_NAME_2016,
               family = nbinom1)

m13 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

m14 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

AICtab(m10, m11, m12, m13, m14)
BICtab(m10, m11, m12, m13, m14)
tab_model(m10, m11, m12, m13, m14)

plot_model(m14, show.values = TRUE, value.offset = .3)
plot_model(m14, type = "re")
sr <- simulateResiduals(m14)
plot(sr)

plot_model(m15, show.values = TRUE, value.offset = .3)
plot_model(m15, type = "re")
sr <- simulateResiduals(m15)
plot(sr)

# ########################################
# IRSAD_d
m20 <- glmmTMB(revenue ~ IRSAD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ 0,
               family = nbinom1)

m21 <- glmmTMB(revenue ~ IRSAD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSAD_d,
               family = nbinom1)

m22 <- glmmTMB(revenue ~ IRSAD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSAD_d + RA_NAME_2016,
               family = nbinom1)

m23 <- glmmTMB(revenue ~ IRSAD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSAD_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

m24 <- glmmTMB(revenue ~ IRSAD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSAD_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

AICtab(m20, m21, m22, m23, m24)
BICtab(m20, m21, m22, m23, m24)
tab_model(m20, m21, m22, m23, m24)

plot_model(m24, show.values = TRUE, value.offset = .3)
plot_model(m24, type = "re")
sr <- simulateResiduals(m24)
plot(sr)

plot_model(m25, show.values = TRUE, value.offset = .3)
plot_model(m25, type = "re")
sr <- simulateResiduals(m25)
plot(sr)

# ########################################
# IER_d
m30 <- glmmTMB(revenue ~ IER_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ 0,
               family = nbinom1)

m31 <- glmmTMB(revenue ~ IER_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IER_d,
               family = nbinom1)

m32 <- glmmTMB(revenue ~ IER_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IER_d + RA_NAME_2016,
               family = nbinom1)

m33 <- glmmTMB(revenue ~ IER_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IER_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

m34 <- glmmTMB(revenue ~ IER_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IER_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

AICtab(m30, m31, m32, m33, m34)
BICtab(m30, m31, m32, m33, m34)
tab_model(m30, m31, m32, m33, m34)

plot_model(m34, show.values = TRUE, value.offset = .3)
plot_model(m34, type = "re")
sr <- simulateResiduals(m34)
plot(sr)

plot_model(m35, show.values = TRUE, value.offset = .3)
plot_model(m35, type = "re")
sr <- simulateResiduals(m35)
plot(sr)

# ########################################
# IEO_d
m40 <- glmmTMB(revenue ~ IEO_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ 0,
               family = nbinom1)

m49 <- glmmTMB(revenue ~ IEO_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ 1,
               family = nbinom1)

m41 <- glmmTMB(revenue ~ IEO_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IEO_d,
               family = nbinom1)

m42 <- glmmTMB(revenue ~ IEO_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IEO_d + RA_NAME_2016,
               family = nbinom1)

m43 <- glmmTMB(revenue ~ IEO_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IEO_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

m44 <- glmmTMB(revenue ~ IEO_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ STE_NAME16 + IEO_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

m45 <- glmmTMB(revenue ~ IEO_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ STE_NAME16 + IEO_d + RA_NAME_2016 + coast_bin,
               family = nbinom2)


AICtab(m40, m49, m41, m42, m43, m44, m45)
BICtab(m40, m49, m41, m42, m43, m44, m45)
tab_model(m40, m49, m41, m42, m43, m44, m45)

plot_model(m43, show.values = TRUE, value.offset = .3)
plot_model(m43, type = "re")
sr <- simulateResiduals(m43)
plot(sr)


m43_d1 <- drop1(m43, test="Chisq")

m43_dredge <- MuMIn::dredge(m43)
plot(m43_dredge)
model.avg(m43_dredge)

# ########################################
tab_model(m13, m23, m33, m43)
