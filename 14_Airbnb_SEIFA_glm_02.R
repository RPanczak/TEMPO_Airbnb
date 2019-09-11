# ########################################
# Testing GLMMadaptive on Airbnb data
# SA1 level input
# no time - aggregated 11mo of data used
# state as RE
# various specs of adjustment
# ########################################

set.seed(12345)
options(scipen = 999)

# ########################################
library(GLMMadaptive)
library(magrittr)
library(dplyr)
library(ggplot2)

# https://cran.r-project.org/web/packages/GLMMadaptive/index.html
# https://cran.r-project.org/web/packages/GLMMadaptive/vignettes/Methods_MixMod.html
# https://cran.r-project.org/web/packages/GLMMadaptive/vignettes/Goodness_of_Fit.html

resids_plot <- function (object, y, nsim = 1000,
                         type = c("subject_specific", "mean_subject"),
                         integerResponse = NULL) {
  if (!inherits(object, "MixMod"))
    stop("this function works for 'MixMod' objects.\n")
  type <- match.arg(type)
  if (is.null(integerResponse)) {
    integer_families <- c("binomial", "poisson", "negative binomial",
                          "zero-inflated poisson", "zero-inflated negative binomial", 
                          "hurdle poisson", "hurdle negative binomial")
    numeric_families <- c("hurdle log-normal", "beta", "hurdle beta")
    if (object$family$family %in% integer_families) {
      integerResponse <- TRUE
    } else if (object$family$family %in% numeric_families) {
      integerResponse <- FALSE
    } else {
      stop("non build-in family object; you need to specify the 'integerResponse',\n",
           "\targument indicating whether the outcome variable is integer or not.\n")
    }
  }
  sims <- simulate(object, nsim = nsim, type = type)
  fits <- fitted(object, type = type)
  dharmaRes <- DHARMa::createDHARMa(simulatedResponse = sims, observedResponse = y, 
                                    fittedPredictedResponse = fits, 
                                    integerResponse = integerResponse)
  DHARMa:::plot.DHARMa(dharmaRes, quantreg = FALSE)
}

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

m11 <- mixed_model(revenue ~ IRSD_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSD_d,
                   zi_random = ~ 1 | STE_NAME16)

m12 <- mixed_model(revenue ~ IRSD_d + RA_NAME_2016, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSD_d, # + RA_NAME_2016,
                   zi_random = ~ 1 | STE_NAME16)

m13 <- mixed_model(revenue ~ IRSD_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSD_d, # + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

anova(m11, m13)
anova(m11, m12)
anova(m12, m14)

exp(confint(m12))
exp(ranef(m12))

resids_plot(m12, airbnb_sa1$revenue)

# ########################################

m21 <- mixed_model(revenue ~ IRSAD_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSAD_d,
                   zi_random = ~ 1 | STE_NAME16)

m22 <- mixed_model(revenue ~ IRSAD_d + RA_NAME_2016, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSAD_d, # + RA_NAME_2016,
                   zi_random = ~ 1 | STE_NAME16)

m23 <- mixed_model(revenue ~ IRSAD_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IRSAD_d, # + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

anova(m21, m22)
anova(m22, m23)

exp(confint(m22))
exp(ranef(m22))

resids_plot(m22, airbnb_sa1$revenue)


# ########################################

m31 <- mixed_model(revenue ~ IER_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IER_d,
                   zi_random = ~ 1 | STE_NAME16)

m32 <- mixed_model(revenue ~ IER_d + RA_NAME_2016, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IER_d, # + RA_NAME_2016,
                   zi_random = ~ 1 | STE_NAME16)

m33 <- mixed_model(revenue ~ IER_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IER_d, # + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

anova(m31, m32)
anova(m32, m33)

exp(confint(m32))
exp(ranef(m32))

resids_plot(m32, airbnb_sa1$revenue)

# ########################################
m40 <- mixed_model(revenue ~ IEO_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d)

m41 <- mixed_model(revenue ~ IEO_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d,
                   zi_random = ~ 1 | STE_NAME16)

m42 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d, # + RA_NAME_2016,
                   zi_random = ~ 1 | STE_NAME16)

m43 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d, # + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

m44 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

m45 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d + RA_NAME_2016 + coast_bin)

m46 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d)

m47 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d + RA_NAME_2016 + coast_bin)

m48 <- mixed_model(revenue ~ IEO_d, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = negative.binomial())

anova(m40, m46)
anova(m40, m47)
anova(m40, m48)

anova(m40, m41)
anova(m41, m42)
anova(m42, m43)
anova(m43, m44)

exp(confint(m40))
exp(ranef(m40))

resids_plot(m40, airbnb_sa1$revenue)
plot(airbnb_sa1$revenue, fitted(m40, type = "subject_specific"))

# ########################################
anova(m12, m22)
anova(m22, m32)
anova(m32, m42)
