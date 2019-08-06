# ########################################
# Testing INLA on Airbnb data
# SA2 level input
# no attempt to add state
# separate rw for each decile
# ########################################

set.seed(12345)
options(scipen = 999)

# ########################################
# INLA
library("INLA")
library("ggplot2")
# install.packages('INLAutils')
# library(devtools)
# install_github('timcdlucas/INLAutils')
library(INLAutils)

# #### data
# airbnb_sa1 <- readRDS(file = "./data/airdna/clean/airbnb_sa1.rds")
# ggplot(data=airbnb_sa1,
#        aes(x=reporting_month, y=cumulative,
#            group=SA1_MAIN16)) +
#   geom_line(aes(alpha=0.1)) +
#   theme_minimal() + xlab("") + ylab("Airbnb revenue") +
#   facet_wrap(~IRSD_d) +
#   geom_smooth(aes(group=IRSD_d)) + scale_y_sqrt()

airbnb_sa2 <- readRDS(file = "./data/airdna/clean/airbnb_sa2.rds")

airbnb_sa2$group <- ave(airbnb_sa2$SA2_MAIN16, airbnb_sa2$reporting_month, FUN = seq_along)
airbnb_sa2$time <- ave(airbnb_sa2$SA2_MAIN16, airbnb_sa2$SA2_MAIN16, FUN = seq_along)

airbnb_sa2$time2 <- airbnb_sa2$time
airbnb_sa2$time3 <- airbnb_sa2$time
airbnb_sa2$time4 <- airbnb_sa2$time
airbnb_sa2$time5 <- airbnb_sa2$time
airbnb_sa2$time6 <- airbnb_sa2$time
airbnb_sa2$time7 <- airbnb_sa2$time
airbnb_sa2$time8 <- airbnb_sa2$time
airbnb_sa2$time9 <- airbnb_sa2$time
airbnb_sa2$time10 <- airbnb_sa2$time

airbnb_sa2$IRSD_d_1 <- as.numeric(airbnb_sa2$IRSD_d == 1)
airbnb_sa2$IRSD_d_2 <- as.numeric(airbnb_sa2$IRSD_d == 2)
airbnb_sa2$IRSD_d_3 <- as.numeric(airbnb_sa2$IRSD_d == 3)
airbnb_sa2$IRSD_d_4 <- as.numeric(airbnb_sa2$IRSD_d == 4)
airbnb_sa2$IRSD_d_5 <- as.numeric(airbnb_sa2$IRSD_d == 5)
airbnb_sa2$IRSD_d_6 <- as.numeric(airbnb_sa2$IRSD_d == 6)
airbnb_sa2$IRSD_d_7 <- as.numeric(airbnb_sa2$IRSD_d == 7)
airbnb_sa2$IRSD_d_8 <- as.numeric(airbnb_sa2$IRSD_d == 8)
airbnb_sa2$IRSD_d_9 <- as.numeric(airbnb_sa2$IRSD_d == 9)
airbnb_sa2$IRSD_d_10 <- as.numeric(airbnb_sa2$IRSD_d == 10)

# temp <- subset(airbnb_sa2, (group > 50 & group <= 100) | (group > 559 & group <= 609))
# length(unique(temp$SA2_MAIN16))
# table(temp$SA2_MAIN16)
# table(temp$STE_CODE16)
# table(temp$SA2_MAIN16, temp$STE_CODE16)
# 
# ggplot(data=temp,
#        aes(x=cumulative)) +
#   geom_histogram() +
#   theme_minimal() + xlab("Cumulative monthly income") + ylab("Property-month counts")
# 
# # cumulative
# ggplot(data=temp,
#        aes(x=time, y=cumulative,
#            color=STE_NAME16,
#            group=SA2_MAIN16)) +
#   geom_line() +
#   theme_minimal() + xlab("") + ylab("Airbnb revenue")
# 
# ggplot(data=temp,
#        aes(x=time, y=cumulative,
#            group=SA2_MAIN16)) +
#   geom_line() +
#   theme_minimal() + xlab("") + ylab("Airbnb revenue") +
#   facet_wrap(~IRSD_d)
# 
# # revenue
# ggplot(data=temp,
#        aes(x=time, y=revenue,
#            color=STE_NAME16,
#            group=SA2_MAIN16)) +
#   geom_line() +
#   theme_minimal() + xlab("") + ylab("Airbnb revenue")
# 
# ggplot(data=temp,
#        aes(x=time, y=revenue,
#            group=SA2_MAIN16)) +
#   geom_line() +
#   theme_minimal() + xlab("") + ylab("Airbnb revenue") +
#   facet_wrap(~IRSD_d)

# ####
# Jeffreys prior 
a1 <- 5e-5
b1 <- 5e-5
lgprior1 <- list(prec = list(param = c(a1, b1)))

# Gelman prior
a2 <- -0.5
b2 <- 5e-5
lgprior2 <- list(prec = list(param = c(a2, b2)))

# iid prior 
# Schrödle & Held 2010 & Blangiardo et al 2013
a0 <- 1
b0 <- 0.1
prior.nu <- list(prec = list(param = c(a0, b0)))

# intercept & fixed
inla.set.control.fixed.default() 

# intercept ~ N(0,0) 
# other fixed effects ~ N(0, 0.001) 
# 
# where the format is N(mean, precision) 
# precision = inverse of the variance. 

# ########################################
# ####
# m1_IRSD_d <- readRDS(file = "./res/INLA/IRSD_d/m1_IRSD_d.rds")
# m2_IRSD_d <- readRDS(file = "./res/INLA/IRSD_d/m2_IRSD_d.rds")
# m3_IRSD_d <- readRDS(file = "./res/INLA/IRSD_d/m3_IRSD_d.rds")

# ########################################
# ####
f1_IRSD_d = cumulative ~ f(time, model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(SA2_MAIN16, model="iid", param = prior.nu) 

m1_IRSD_d <- inla(f1_IRSD_d, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m1_IRSD_d)
# autoplot(m1_IRSD_d)
summary(exp(m1_IRSD_d$summary.random$SA2_MAIN16$`0.5quant`))
m1_IRSD_d$summary.hyperpar

m1_IRSD_dvarTI <- inla.emarginal(function(x) 1/x, m1_IRSD_d$marginals.hyper$"Precision for time")
m1_IRSD_dvarSA <- inla.emarginal(function(x) 1/x, m1_IRSD_d$marginals.hyper$"Precision for SA2_MAIN16")

# m1_IRSD_d$summary.random$time
# m1_IRSD_d$summary.fitted.values
m1_IRSD_d$summary.fixed
exp(m1_IRSD_d$summary.fixed$`0.5quant`)

# plot <- as.data.frame(m1_IRSD_d$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`)
# plot$`0.025quant` <- exp(plot$`0.025quant`)
# plot$`0.975quant` <- exp(plot$`0.975quant`)
# 
# ggplot(plot) +
#   geom_line(aes(ID, `0.5quant`)) +
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m1_IRSD_dh <- inla.hyperpar(m1_IRSD_d)


# ####
f2_IRSD_d = cumulative ~ f(time, model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(SA2_MAIN16, model="iid", param = prior.nu) + 
  as.factor(IRSD_d)

m2_IRSD_d <- inla(f2_IRSD_d, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m2_IRSD_d)
# autoplot(m2_IRSD_d)
summary(exp(m2_IRSD_d$summary.random$SA2_MAIN16$`0.5quant`))
m2_IRSD_d$summary.hyperpar

m2_IRSD_dvarTI <- inla.emarginal(function(x) 1/x, m2_IRSD_d$marginals.hyper$"Precision for time")
m2_IRSD_dvarSA <- inla.emarginal(function(x) 1/x, m2_IRSD_d$marginals.hyper$"Precision for SA2_MAIN16")

# m2_IRSD_d$summary.random$time
# m2_IRSD_d$summary.fitted.values
m2_IRSD_d$summary.fixed
exp(m2_IRSD_d$summary.fixed$`0.5quant`)

# plot <- as.data.frame(m2_IRSD_d$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`)
# plot$`0.025quant` <- exp(plot$`0.025quant`)
# plot$`0.975quant` <- exp(plot$`0.975quant`)
# 
# ggplot(plot) +
#   geom_line(aes(ID, `0.5quant`)) +
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m2_IRSD_dh <- inla.hyperpar(m2_IRSD_d)


# ########################################
# ####
f3_IRSD_d = cumulative ~ 
  f(time,   IRSD_d_1,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time2,  IRSD_d_2,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time3,  IRSD_d_3,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time4,  IRSD_d_4,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time5,  IRSD_d_5,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time6,  IRSD_d_6,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time7,  IRSD_d_7,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time8,  IRSD_d_8,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time9,  IRSD_d_9,  model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(time10, IRSD_d_10, model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(SA2_MAIN16, model="iid", param = prior.nu) + 
  as.factor(IRSD_d)

m3_IRSD_d <- inla(f3_IRSD_d, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m3_IRSD_d)
# autoplot(m3_IRSD_d)
summary(exp(m3_IRSD_d$summary.random$SA2_MAIN16$`0.5quant`))
m3_IRSD_d$summary.hyperpar

m3_IRSD_dvarTI <- inla.emarginal(function(x) 1/x, m3_IRSD_d$marginals.hyper$"Precision for time")
m3_IRSD_dvarSA <- inla.emarginal(function(x) 1/x, m3_IRSD_d$marginals.hyper$"Precision for SA2_MAIN16")

m3_IRSD_d$summary.fixed
exp(m3_IRSD_d$summary.fixed$`0.5quant`)
# m3_IRSD_d$summary.random$time
# m3_IRSD_d$summary.fitted.values

# plot <- as.data.frame(m3_IRSD_d$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`) 
# plot$`0.025quant` <- exp(plot$`0.025quant`)  
# plot$`0.975quant` <- exp(plot$`0.975quant`)  
# 
# ggplot(plot) +   
#   geom_line(aes(ID, `0.5quant`)) +   
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +   
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m3_IRSD_dh <- inla.hyperpar(m3_IRSD_d)

plot(m3_IRSD_d$summary.random$time$ID, exp(m3_IRSD_d$summary.random$time$mean), type = "l", col = "green")
lines(m3_IRSD_d$summary.random$time$ID, exp(m3_IRSD_d$summary.random$time5$mean), col = "orange")
lines(m3_IRSD_d$summary.random$time$ID, exp(m3_IRSD_d$summary.random$time10$mean), col = "red")

# ########################################
# comparing models
c(m1_IRSD_dvarTI, m2_IRSD_dvarTI, m3_IRSD_dvarTI)
c(m1_IRSD_dvarSA, m2_IRSD_dvarSA, m3_IRSD_dvarSA)

dotchart(x = c(m1_IRSD_d$dic$dic, m2_IRSD_d$dic$dic, m3_IRSD_d$dic$dic), labels = c("m1_IRSD_d", "m2_IRSD_d", "m3_IRSD_d"))
dotchart(x = c(m1_IRSD_d$waic$waic, m2_IRSD_d$waic$waic, m3_IRSD_d$waic$waic), labels = c("m1_IRSD_d", "m2_IRSD_d", "m3_IRSD_d"))

saveRDS(m1_IRSD_d, file = "./res/INLA/IRSD_d/m1_IRSD_d.rds")
saveRDS(m2_IRSD_d, file = "./res/INLA/IRSD_d/m2_IRSD_d.rds")
saveRDS(m3_IRSD_d, file = "./res/INLA/IRSD_d/m3_IRSD_d.rds")

# ########################################
# quick maps
SA2 %<>% left_join(select(m1_IRSD_d$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m1_IRSD_d = mean))
SA2 %<>% left_join(select(m2_IRSD_d$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m2_IRSD_d = mean))
SA2 %<>% left_join(select(m3_IRSD_d$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m3_IRSD_d = mean)) %>%
  mutate(exp_m1_IRSD_d = exp(exp_m1_IRSD_d),
         exp_m2_IRSD_d = exp(exp_m2_IRSD_d),
         exp_m3_IRSD_d = exp(exp_m3_IRSD_d))




