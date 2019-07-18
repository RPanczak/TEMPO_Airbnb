# ########################################
# Testing glm on Airbnb data
# SA1 level input
# no time - only Aug '16 data used
# state as factor
# various variables for adjustment
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

# airbnb_sa1 <- readRDS(file = "./data/airdna/clean/airbnb_sa1.rds") %>% 
#   filter(reporting_month == as.Date("2016-08-01")) %>% 
#   select(-reporting_month) %>% 
#   mutate(
#     IRSD_d = factor(IRSD_d),
#     IRSAD_d = factor(IRSAD_d), 
#     IER_d = factor(IER_d),
#     IEO_d = factor(IEO_d),
#     STE_NAME16 = factor(STE_NAME16),
#     SOS_NAME_2016 = factor(SOS_NAME_2016),
#     RA_NAME_2016 = factor(RA_NAME_2016),
#     coast_bin = factor(coast_bin, levels = c(0, 1), labels = c("No", "Yes"))
#   )

# airbnb_sa1 <- readRDS(file = "./data/airdna/clean/airbnb_sa1.rds") %>% 
airbnb_sa1 %<>% 
  filter(reporting_month >= as.Date("2016-03-01") & reporting_month >= as.Date("2017-02-01")) %>% 
  group_by(SA1_MAIN16) %>% 
  mutate(
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
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(
    IRSD_d = factor(IRSD_d),
    IRSAD_d = factor(IRSAD_d), 
    IER_d = factor(IER_d),
    IEO_d = factor(IEO_d),
    STE_NAME16 = factor(STE_NAME16),
    SOS_NAME_2016 = factor(SOS_NAME_2016),
    RA_NAME_2016 = factor(RA_NAME_2016),
    coast_bin = factor(coast_bin, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>% 
  select(-reporting_month, -cumulative) 


# ref categories for factors
# table(airbnb_sa1$STE_NAME16)  
airbnb_sa1$STE_NAME16 <- relevel(airbnb_sa1$STE_NAME16, ref = "New South Wales")

# table(airbnb_sa1$SOS_NAME_2016)  
airbnb_sa1$SOS_NAME_2016 <- relevel(airbnb_sa1$SOS_NAME_2016, ref = "Major Urban")

# table(airbnb_sa1$RA_NAME_2016)  
airbnb_sa1$RA_NAME_2016 <- relevel(airbnb_sa1$RA_NAME_2016, ref = "Major Cities of Australia")

# ggplot(data=airbnb_sa1,
#        aes(x=revenue)) +
#   geom_histogram(binwidth = 10000) +
#   theme_minimal() + xlab("Cumulative monthly income") + ylab("Property-month counts")

# airbnb_sa1 %>% 
#   select(-starts_with("SA2")) %>% 
#   view_df(show.na = TRUE, 
#         show.type = TRUE, 
#         show.frq = TRUE, 
#         show.prc = TRUE)

# ########################################
# ########################################
# ########################################
# desc 
airbnb_sa1 %>% 
  group_by(IRSD_d) %>% 
  summarize(Mean = mean(revenue),
            Sd = sd(revenue))

ggplot(airbnb_sa1, aes(x=IRSD_d, y=revenue)) +
  geom_boxplot() +
  scale_y_sqrt()

# zeroes
airbnb_sa1 %<>% 
  mutate(zero = ifelse(revenue == 0, 1, 0))


# IRSD_d
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=IRSD_d)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = IRSD_d)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + IRSD_d, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# IRSAD_d
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=IRSAD_d)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = IRSAD_d)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + IRSAD_d, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# IER_d
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=IER_d)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = IER_d)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + IER_d, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# IEO_d
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=IEO_d)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = IEO_d)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + IEO_d, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# STE_NAME16
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=STE_NAME16)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = STE_NAME16)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + STE_NAME16, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# SOS_NAME_2016
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=SOS_NAME_2016)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = SOS_NAME_2016)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + SOS_NAME_2016, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# RA_NAME_2016
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=RA_NAME_2016)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = RA_NAME_2016)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + RA_NAME_2016, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# coast_bin
ggplot(airbnb_sa1, aes(x=zero)) +
  geom_bar(aes(fill=coast_bin)) 

ggplot(airbnb_sa1, aes(x = factor(zero), fill = coast_bin)) +
  geom_bar(position="fill")

model <- glm(zero ~ -1 + coast_bin, data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))


# together
model <- glm(zero ~ -1 + IRSD_d + STE_NAME16 + SOS_NAME_2016 + coast_bin, 
             data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))

model <- glm(zero ~ -1 + IRSD_d + STE_NAME16 + RA_NAME_2016 + coast_bin, 
             data = airbnb_sa1, family = "binomial")
exp(cbind(OR = coef(model), confint(model)))

# ########################################
# ########################################
# ########################################
m11 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d,
               family = nbinom1)

m12 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m13 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + SOS_NAME_2016,
               family = nbinom1)

m14 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + SOS_NAME_2016 + coast_bin,
               family = nbinom1)

m15 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~ IRSD_d + RA_NAME_2016 + coast_bin,
               family = nbinom1)

AICtab(m11, m12, m14)
AICtab(m11, m13, m15)
BICtab(m11, m12, m14)
BICtab(m11, m13, m15)
tab_model(m11, m12, m13, m14, m15)

plot_model(m14, show.values = TRUE, value.offset = .3)
plot_model(m14, type = "re")
sr <- simulateResiduals(m14)
plot(sr)

plot_model(m15, show.values = TRUE, value.offset = .3)
plot_model(m15, type = "re")
sr <- simulateResiduals(m15)
plot(sr)

# ########################################
m21 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m22 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m23 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m24 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m25 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

AICtab(m21, m22, m24)
AICtab(m21, m23, m25)
tab_model(m21, m22, m23, m24, m25)

plot_model(m24, show.values = TRUE, value.offset = .3)
plot_model(m24, type = "re")
sr <- simulateResiduals(m24)
plot(sr)

plot_model(m25, show.values = TRUE, value.offset = .3)
plot_model(m25, type = "re")
sr <- simulateResiduals(m25)
plot(sr)

# ########################################
m31 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m32 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m33 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m34 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m35 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

AICtab(m31, m32, m34)
AICtab(m31, m33, m35)
tab_model(m31, m32, m33, m34, m35)

plot_model(m34, show.values = TRUE, value.offset = .3)
plot_model(m34, type = "re")
sr <- simulateResiduals(m34)
plot(sr)

plot_model(m35, show.values = TRUE, value.offset = .3)
plot_model(m35, type = "re")
sr <- simulateResiduals(m35)
plot(sr)

# ########################################
m41 <- glmmTMB(revenue ~ IRSD_d +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m42 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m43 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m44 <- glmmTMB(revenue ~ IRSD_d + SOS_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

m45 <- glmmTMB(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin +
                 (1 | STE_NAME16),
               data = airbnb_sa1,
               ziformula = ~1,
               family = nbinom1)

AICtab(m41, m42, m44)
AICtab(m41, m43, m45)
tab_model(m41, m42, m43, m44, m45)

plot_model(m44, show.values = TRUE, value.offset = .3)
plot_model(m44, type = "re")
sr <- simulateResiduals(m44)
plot(sr)

plot_model(m45, show.values = TRUE, value.offset = .3)
plot_model(m45, type = "re")
sr <- simulateResiduals(m45)
plot(sr)

# ########################################
tab_model(m14, m24, m34, m44)
tab_model(m15, m25, m35, m45)
