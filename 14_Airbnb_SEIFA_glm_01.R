# ########################################
# Testing glm on Airbnb data
# SA1 level input
# no time - aggregated 11mo of data used
# state as level
# various variables for adjustment
# ########################################

set.seed(12345)
options(scipen = 999)

# ########################################
p_load(dplyr)
p_load(ggplot2)
p_load(sjPlot)
p_load(lme4)

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
m11 <- glmer.nb(revenue ~ IRSD_d  + 
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m12 <- glmer.nb(revenue ~ IRSD_d  + 
                  STE_NAME16 + SOS_NAME_2016 +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m13 <- glmer.nb(revenue ~ IRSD_d  + 
                  STE_NAME16 + RA_NAME_2016 +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m14 <- glmer.nb(revenue ~ IRSD_d  + 
                  STE_NAME16 + SOS_NAME_2016 + coast_bin +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m15 <- glmer.nb(revenue ~ IRSD_d  + 
                  STE_NAME16 + RA_NAME_2016 + coast_bin +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

anova(m10, m11, m12, m14)
anova(m10, m11, m13, m15)
tab_model(m10, m11, m12, m13, m14, m15, 
          show.intercept = FALSE, show.aic = TRUE, show.dev = TRUE)

# est_m15 <- as.data.frame(cbind(Estimate = coef(m15), confint(m15)))
# est_m15$Estimate <- exp(est_m15$Estimate)
# est_m15$`2.5 %` <- exp(est_m15$`2.5 %`)
# est_m15$`97.5 %` <- exp(est_m15$`97.5 %`)

# airbnb_sa1$m15_phat <- predict(m15, airbnb_sa1, type = "response")
# plot(airbnb_sa1$m15_phat, airbnb_sa1$revenue)

plot_model(m14, show.values = TRUE, value.offset = .3)
plot_model(m15, show.values = TRUE, value.offset = .3)

# plot_model(m14, type = "resid")
# plot_model(m15, type = "resid")



# ########################################
m41 <- glmer.nb(revenue ~ IEO_d  + 
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m42 <- glmer.nb(revenue ~ IEO_d  + 
                  STE_NAME16 + SOS_NAME_2016 +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m43 <- glmer.nb(revenue ~ IEO_d  + 
                  STE_NAME16 + RA_NAME_2016 +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m44 <- glmer.nb(revenue ~ IEO_d  + 
                  STE_NAME16 + SOS_NAME_2016 + coast_bin +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

m45 <- glmer.nb(revenue ~ IEO_d  + 
                  STE_NAME16 + RA_NAME_2016 + coast_bin +
                  (1 | STE_NAME16), 
                data = airbnb_sa1) 

anova(m40, m41, m42, m44)
anova(m40, m41, m43, m45)
tab_model(m40, m41, m42, m43, m44, m45, 
          show.intercept = FALSE, show.aic = TRUE, show.dev = TRUE)

# est_m45 <- as.data.frame(cbind(Estimate = coef(m45), confint(m45)))
# est_m45$Estimate <- exp(est_m45$Estimate)
# est_m45$`2.5 %` <- exp(est_m45$`2.5 %`)
# est_m45$`97.5 %` <- exp(est_m45$`97.5 %`)

# airbnb_sa1$m45_phat <- predict(m45, airbnb_sa1, type = "response")
# plot(airbnb_sa1$m45_phat, airbnb_sa1$revenue)

plot_model(m44, show.values = TRUE, value.offset = .3)
plot_model(m45, show.values = TRUE, value.offset = .3)

# plot_model(m44, type = "resid")
# plot_model(m45, type = "resid")

# ########################################
tab_model(m14, m24, m34, m44)
tab_model(m15, m25, m35, m45)
