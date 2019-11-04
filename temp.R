# ML
p_load(lme4)
p_load(nlme)

data <- APM_sa2 %>% 
  filter(property_type == "Unit") %>% 
  group_by(SA2_MAIN16) %>% 
  mutate(time = row_number())

# unconditional model
# u_m1 <- lme(rent_50p_int ~ 1, random = ~ 1 | SA2_MAIN16, data = data)

# model
u_m2 <- lme(rent_50p_int ~ time, random = ~ 1 | SA2_MAIN16, data = data)
summary(u_m2)

# unconditional growth model
u_m3 <- lme(rent_50p_int ~ time, random = ~ time | SA2_MAIN16, data = data)
summary(u_m3)

anova(u_m2, u_m3) 

u_m4 <- lme(rent_50p_int ~ time, random = ~ 1 | SA2_MAIN16, 
            correlation = corAR1(), data = data)
summary(u_m4)

anova(u_m2, u_m4) 

u_m5 <- lme(rent_50p_int ~ time, random = ~ time | SA2_MAIN16, 
            correlation = corAR1(form = ~ time | SA2_MAIN16), data = data)
summary(u_m5)

anova(u_m4, u_m5) 


# unconditional model
# u_m1 <- lmer(rent_50p_int ~ 1 + (1 | SA2_MAIN16), data = data)

# model
u_m2 <- lmer(rent_50p_int ~ time + (1 | SA2_MAIN16), data = data)
summary(u_m2)
# confint(u_m2)

# unconditional growth model
u_m3 <- lmer(rent_50p_int ~ time + (time | SA2_MAIN16), data = data)
summary(u_m3)

anova(u_m2, u_m3) 


# INLA
p_load(INLA)

f1 <- rent_50p_int ~ 
  f(SA2_MAIN16, model = "iid", constr = TRUE) + 
  f(time, model = "rw1", constr = TRUE)

m1 <- inla(f1, data = data)

summary(m1)
plot(m1)

f2 <- rent_50p_int ~ 
  f(SA2_MAIN16, model = "iid", constr = TRUE) + 
  f(time, model = "rw2", constr = TRUE)

m2 <- inla(f2, data = data)

summary(m2)
plot(m2)
