# ########################################
# Testing glm on Airbnb data
# SA1 level input
# no time - aggregated 11mo of data used
# state as factor
# various variables for adjustment
# ########################################

set.seed(12345)
options(scipen = 999)

# ########################################
library(dplyr)
library(ggplot2)
library(sjPlot)
library(MASS)

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
# desc 
airbnb_sa1 %>%
  dplyr::select(-starts_with("SA2"), -starts_with("SA1"), -STE_CODE16) %>%
  view_df(show.na = TRUE,
          show.type = TRUE,
          show.frq = TRUE,
          show.prc = TRUE)

ggplot(data=airbnb_sa1,
       aes(x=revenue)) +
  geom_histogram(binwidth = 10000) +
  theme_minimal() + 
  xlab("Cumulative income") + ylab("No of SA1s")

airbnb_sa1 %>% 
  group_by(IRSD_d) %>% 
  summarize(Mean = mean(revenue),
            Sd = sd(revenue),
            Median = median(revenue),
            Sum = sum(revenue))

airbnb_sa1 %>% 
  group_by(IRSAD_d) %>% 
  summarize(Mean = mean(revenue),
            Sd = sd(revenue),
            Median = median(revenue),
            Sum = sum(revenue))

airbnb_sa1 %>% 
  group_by(IER_d) %>% 
  summarize(Mean = mean(revenue),
            Sd = sd(revenue),
            Median = median(revenue),
            Sum = sum(revenue))

airbnb_sa1 %>% 
  group_by(IEO_d) %>% 
  summarize(Mean = mean(revenue),
            Sd = sd(revenue),
            Median = median(revenue),
            Sum = sum(revenue))

airbnb_sa1 %>% 
  ggplot(aes(x=IEO_d, y=revenue)) +
  geom_boxplot() +
  scale_y_sqrt()

airbnb_sa1 %>% 
  group_by(IEO_d) %>% 
  summarise(Sum = sum(revenue)/1000000) %>% 
  ggplot(aes(x=IEO_d, y=Sum)) +
  geom_col() 

airbnb_sa1 %>% 
  ggplot(aes(x=SA4_GCC_NAME16, y=revenue)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_sqrt()

airbnb_sa1 %>% 
  group_by(SA4_GCC_NAME16) %>% 
  summarise(Sum = sum(revenue)/1000000) %>% 
  ggplot(aes(x=SA4_GCC_NAME16, y=Sum)) +
  geom_col() +
  coord_flip()

# ########################################
# ########################################
# ########################################
m10 <- glm.nb(revenue ~ IRSD_d, 
              data = airbnb_sa1) 

m11 <- glm.nb(revenue ~ IRSD_d +
                STE_NAME16, 
              data = airbnb_sa1) 

m12 <- glm.nb(revenue ~ IRSD_d + 
                STE_NAME16 + SOS_NAME_2016, 
              data = airbnb_sa1)

m13 <- glm.nb(revenue ~ IRSD_d + 
                STE_NAME16 + RA_NAME_2016, 
              data = airbnb_sa1) 

m14 <- glm.nb(revenue ~ IRSD_d + 
                STE_NAME16 + SOS_NAME_2016 + coast_bin, 
              data = airbnb_sa1) 

m15 <- glm.nb(revenue ~ IRSD_d + 
                STE_NAME16 + RA_NAME_2016 + coast_bin, 
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
m20 <- glm.nb(revenue ~ IRSAD_d, data = airbnb_sa1) 

m21 <- glm.nb(revenue ~ IRSAD_d +
                STE_NAME16, 
              data = airbnb_sa1) 

m22 <- glm.nb(revenue ~ IRSAD_d + 
                STE_NAME16 + SOS_NAME_2016, 
              data = airbnb_sa1)

m23 <- glm.nb(revenue ~ IRSAD_d + 
                STE_NAME16 + RA_NAME_2016, 
              data = airbnb_sa1) 

m24 <- glm.nb(revenue ~ IRSAD_d + 
                STE_NAME16 + SOS_NAME_2016 + coast_bin, 
              data = airbnb_sa1) 

m25 <- glm.nb(revenue ~ IRSAD_d + 
                STE_NAME16 + RA_NAME_2016 + coast_bin, 
              data = airbnb_sa1)

anova(m20, m21, m22, m24)
anova(m20, m21, m23, m25)
tab_model(m20, m21, m22, m23, m24, m25,
          show.intercept = FALSE, show.aic = TRUE, show.dev = TRUE)

# est_m2 <- as.data.frame(cbind(Estimate = coef(m2), confint(m2)))
# est_m2$Estimate <- exp(est_m2$Estimate)
# est_m2$`2.5 %` <- exp(est_m2$`2.5 %`)
# est_m2$`97.5 %` <- exp(est_m2$`97.5 %`)

# airbnb_sa1$m25_phat <- predict(m25, airbnb_sa1, type = "response")
# plot(airbnb_sa1$m25_phat, airbnb_sa1$revenue)

plot_model(m24, show.values = TRUE, value.offset = .3)
plot_model(m25, show.values = TRUE, value.offset = .3)

# ########################################
m30 <- glm.nb(revenue ~ IER_d, 
              data = airbnb_sa1) 

m31 <- glm.nb(revenue ~ IER_d +
                STE_NAME16, 
              data = airbnb_sa1) 

m32 <- glm.nb(revenue ~ IER_d + 
                STE_NAME16 + SOS_NAME_2016, 
              data = airbnb_sa1)

m33 <- glm.nb(revenue ~ IER_d + 
                STE_NAME16 + RA_NAME_2016, 
              data = airbnb_sa1) 

m34 <- glm.nb(revenue ~ IER_d + 
                STE_NAME16 + SOS_NAME_2016 + coast_bin, 
              data = airbnb_sa1) 

m35 <- glm.nb(revenue ~ IER_d + 
                STE_NAME16 + RA_NAME_2016 + coast_bin, 
              data = airbnb_sa1)

anova(m30, m31, m32, m34)
anova(m30, m31, m33, m35)
tab_model(m30, m31, m32, m33, m34, m35,
          show.intercept = FALSE, show.aic = TRUE, show.dev = TRUE)

# est_m3 <- as.data.frame(cbind(Estimate = coef(m3), confint(m3)))
# est_m3$Estimate <- exp(est_m3$Estimate)
# est_m3$`2.5 %` <- exp(est_m3$`2.5 %`)
# est_m3$`97.5 %` <- exp(est_m3$`97.5 %`)

# airbnb_sa1$m35_phat <- predict(m35, airbnb_sa1, type = "response")
# plot(airbnb_sa1$m35_phat, airbnb_sa1$revenue)

plot_model(m34, show.values = TRUE, value.offset = .3)
plot_model(m35, show.values = TRUE, value.offset = .3)

# ########################################
m40 <- glm.nb(revenue ~ IEO_d, 
              data = airbnb_sa1) 

m41 <- glm.nb(revenue ~ IEO_d +
                STE_NAME16, 
              data = airbnb_sa1) 

m42 <- glm.nb(revenue ~ IEO_d + 
                STE_NAME16 + SOS_NAME_2016, 
              data = airbnb_sa1)

m43 <- glm.nb(revenue ~ IEO_d + 
                STE_NAME16 + RA_NAME_2016, 
              data = airbnb_sa1) 

m44 <- glm.nb(revenue ~ IEO_d + 
                STE_NAME16 + SOS_NAME_2016 + coast_bin, 
              data = airbnb_sa1) 

m45 <- glm.nb(revenue ~ IEO_d + 
                STE_NAME16 + RA_NAME_2016 + coast_bin, 
              data = airbnb_sa1)

m46 <- glm.nb(revenue ~ IEO_d + 
                STE_NAME16 + RA_NAME_2016 + coast_bin + SA4_GCC_NAME16, 
              data = airbnb_sa1)

anova(m40, m41, m42, m44)
anova(m40, m41, m43, m45, m46)
tab_model(m40, m41, m42, m43, m44, m45, m46,
          show.intercept = FALSE, show.aic = TRUE, show.dev = TRUE)

# est_m4 <- as.data.frame(cbind(Estimate = coef(m4), confint(m4)))
# est_m4$Estimate <- exp(est_m4$Estimate)
# est_m4$`2.5 %` <- exp(est_m4$`2.5 %`)
# est_m4$`97.5 %` <- exp(est_m4$`97.5 %`)

# airbnb_sa1$m45_phat <- predict(m45, airbnb_sa1, type = "response")
# plot(airbnb_sa1$m45_phat, airbnb_sa1$revenue)

plot_model(m44, show.values = TRUE, value.offset = .3)
plot_model(m45, show.values = TRUE, value.offset = .3)


# predict?
airbnb_sa1$m45 <- predict(m45, type='response')

max <- max(max(airbnb_sa1$m45), max(airbnb_sa1$revenue))

airbnb_sa1 %>% 
  ggplot(aes(x = revenue, y = m45)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = TRUE) +
  theme_light() +
  # coord_fixed(xlim = c(0, max), ylim = c(0, max)) +
  labs(x = "Revenue", y = "Prediction")


# ########################################
# tab_model(m14, m24, m34, m44)
tab_model(m15, m25, m35, m45,
          show.obs = FALSE, show.intercept = FALSE, 
          show.aic = TRUE, show.dev = TRUE)
