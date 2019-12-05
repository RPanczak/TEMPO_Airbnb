# ML

library(pacman)
p_load(lme4, nlme, ggplot2)

# airbnb_sa1 <- readRDS(file = "./data/airdna/clean/airbnb_sa1.rds")

airbnb_sa2 <- readRDS(file = "./data/airdna/clean/airbnb_sa2.rds")

airbnb_sa2$group <- ave(airbnb_sa2$SA2_MAIN16, airbnb_sa2$reporting_month, FUN = seq_along)
airbnb_sa2$time <- ave(airbnb_sa2$SA2_MAIN16, airbnb_sa2$SA2_MAIN16, FUN = seq_along)

temp <- subset(airbnb_sa2, group >50 & group <= 100)
length(unique(temp$group))
# temp <- subset(airbnb_sa2, STE_NAME16 =="Queensland")

ggplot(data=temp, 
       aes(x=time, y=cumulative, 
           # color=STE_NAME16, 
           group=group)) +
  geom_line() +
  theme_minimal() + xlab("") + ylab("Airbnb revenue")

ggplot(data=temp, 
       aes(x=time, y=cumulative, 
           # color=STE_NAME16, 
           group=group)) +
  geom_line() +
  scale_y_sqrt() +
  theme_minimal() + xlab("") + ylab("Airbnb revenue")

# Unconditional model

# lme4
m1a <- lmer(cumulative ~ 1 + (1 | group), data=temp)

# nlme
m1b <- lme(cumulative ~ 1, random = ~ 1 | group, data=temp)

# Unconditional growth model

# lme4
m2a <- lmer(cumulative ~ time + (time | group), data=temp)

# nlme
m2b <- lme(cumulative ~ time, random = ~ time | group, data=temp)

# Quadratic trend

# lme4
m3a <- lmer(cumulative ~ (time + I(time^2)) + 
              (time + I(time^2) | group),
            data=temp)  

# # or use poly()
# m3a <- lmer(cumulative ~ poly(time, 2, raw = TRUE) + 
#               (poly(time, 2, raw = TRUE) | group),
#             data=airbnb_sa2)  

# nlme
m3b <- lme(cumulative ~ (time + I(time^2)),
           random = ~time + I(time^2) | group,
           data=temp)   

# Orthogonal polynomials

# lme4
m4a <- lmer(cumulative ~ poly(time, 2) +
              (poly(time, 2) | group),
            data=temp)

# nlme
m4b <- lme(cumulative ~ poly(time, 2),
           random = ~poly(time, 2) | group,
           data=temp)
