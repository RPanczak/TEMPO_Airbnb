m1 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d,
                   zi_random = ~ 1 | STE_NAME16)

m2 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d + RA_NAME_2016,
                   zi_random = ~ 1 | STE_NAME16)

m3 <- mixed_model(revenue ~ IEO_d + RA_NAME_2016 + coast_bin, 
                   random = ~ 1 | STE_NAME16, 
                   data = airbnb_sa1,
                   family = zi.negative.binomial(), 
                   zi_fixed = ~ IEO_d + RA_NAME_2016 + coast_bin,
                   zi_random = ~ 1 | STE_NAME16)

anova(m1, m2)
anova(m2, m3)
anova(m1, m3)

exp(confint(m2))
exp(ranef(m2))