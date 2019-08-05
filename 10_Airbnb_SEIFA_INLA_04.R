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
# library("ggplot2")
# install.packages('INLAutils')
# library(devtools)
# install_github('timcdlucas/INLAutils')
# library(INLAutils)

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

# temp <- subset(airbnb_sa2, (group > 50 & group <= 100) | (group > 559 & group <= 609))
# length(unique(temp$SA2_MAIN16))
# table(temp$SA2_MAIN16)
# table(temp$STE_CODE16)
# table(temp$SA2_MAIN16, temp$STE_CODE16)

# ggplot(data=temp,
#        aes(x=cumulative)) +
#   geom_histogram() +
#   theme_minimal() + xlab("Cumulative monthly income") + ylab("Property-month counts")

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
f1 = cumulative ~ f(time, model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(SA2_MAIN16, model="iid", param = prior.nu) 

m1 <- inla(f1, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m1)
# autoplot(m1)
summary(exp(m1$summary.random$SA2_MAIN16$`0.5quant`))
m1$summary.hyperpar

m1varTI <- inla.emarginal(function(x) 1/x, m1$marginals.hyper$"Precision for time")
m1varSA <- inla.emarginal(function(x) 1/x, m1$marginals.hyper$"Precision for SA2_MAIN16")

# m1$summary.random$time
# m1$summary.fitted.values
m1$summary.fixed
exp(m1$summary.fixed$`0.5quant`)

# plot <- as.data.frame(m1$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`)
# plot$`0.025quant` <- exp(plot$`0.025quant`)
# plot$`0.975quant` <- exp(plot$`0.975quant`)
# 
# ggplot(plot) +
#   geom_line(aes(ID, `0.5quant`)) +
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m1h <- inla.hyperpar(m1)


# ####
f2 = cumulative ~ f(time, model = "rw1", scale.model = TRUE, hyper = lgprior2) + 
  f(SA2_MAIN16, model="iid", param = prior.nu) + 
  as.factor(IRSD_d)

m2 <- inla(f2, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m2)
# autoplot(m2)
summary(exp(m2$summary.random$SA2_MAIN16$`0.5quant`))
m2$summary.hyperpar

m2varTI <- inla.emarginal(function(x) 1/x, m2$marginals.hyper$"Precision for time")
m2varSA <- inla.emarginal(function(x) 1/x, m2$marginals.hyper$"Precision for SA2_MAIN16")

# m2$summary.random$time
# m2$summary.fitted.values
m2$summary.fixed
exp(m2$summary.fixed$`0.5quant`)

# plot <- as.data.frame(m2$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`)
# plot$`0.025quant` <- exp(plot$`0.025quant`)
# plot$`0.975quant` <- exp(plot$`0.975quant`)
# 
# ggplot(plot) +
#   geom_line(aes(ID, `0.5quant`)) +
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m2h <- inla.hyperpar(m2)


# ########################################
# ####
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

f3 = cumulative ~ 
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

m3 <- inla(f3, 
           family = "nbinomial",
           data = airbnb_sa2,
           control.compute = list(dic = TRUE, waic = TRUE))

summary(m3)
# autoplot(m3)
summary(exp(m3$summary.random$SA2_MAIN16$`0.5quant`))
m3$summary.hyperpar

m3varTI <- inla.emarginal(function(x) 1/x, m3$marginals.hyper$"Precision for time")
m3varSA <- inla.emarginal(function(x) 1/x, m3$marginals.hyper$"Precision for SA2_MAIN16")

m3$summary.fixed
exp(m3$summary.fixed$`0.5quant`)
# m3$summary.random$time
# m3$summary.fitted.values

# plot <- as.data.frame(m3$summary.random$time)
# 
# plot$`0.5quant` <- exp(plot$`0.5quant`) 
# plot$`0.025quant` <- exp(plot$`0.025quant`)  
# plot$`0.975quant` <- exp(plot$`0.975quant`)  
# 
# ggplot(plot) +   
#   geom_line(aes(ID, `0.5quant`)) +   
#   geom_line(aes(ID, `0.025quant`), linetype = "dashed") +   
#   geom_line(aes(ID, `0.975quant`), linetype = "dashed") # + scale_y_log10()

# m3h <- inla.hyperpar(m3)

plot(m3$summary.random$time$ID, exp(m3$summary.random$time$mean), type = "l", col = "green")
lines(m3$summary.random$time$ID, exp(m3$summary.random$time5$mean), col = "orange")
lines(m3$summary.random$time$ID, exp(m3$summary.random$time10$mean), col = "red")

# ########################################
# comparing models
c(m1varTI, m2varTI, m3varTI)
c(m1varSA, m2varSA, m3varSA)

dotchart(x = c(m1$dic$dic, m2$dic$dic, m3$dic$dic), labels = c("m1", "m2", "m3"))
dotchart(x = c(m1$waic$waic, m2$waic$waic, m3$waic$waic), labels = c("m1", "m2", "m3"))

# saveRDS(m1, file = "./res/INLA/04/m1.rds")
# saveRDS(m2, file = "./res/INLA/04/m2.rds")
# saveRDS(m3, file = "./res/INLA/04/m3.rds")

# m1 <- readRDS(file = "./res/INLA/04/m1.rds")
# m2 <- readRDS(file = "./res/INLA/04/m2.rds")
# m3 <- readRDS(file = "./res/INLA/04/m3.rds")

# ########################################
# quick maps
SA2 %<>% left_join(select(m1$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m1 = mean))
SA2 %<>% left_join(select(m2$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m2 = mean))
SA2 %<>% left_join(select(m3$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m3 = mean)) %>%
  mutate(exp_m1 = exp(exp_m1),
         exp_m2 = exp(exp_m2),
         exp_m3 = exp(exp_m3))




