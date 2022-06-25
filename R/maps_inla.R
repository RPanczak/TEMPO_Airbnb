# ########################################
# quick maps

SA2 <- readRDS(file = "./data/geo/SA2_2016_AUST_clean.rds") %>% 
  filter(SA4_CODE16 != "901") %>% 
  select(-starts_with("SA3")) %>% 
  select(-starts_with("SA4")) %>% 
  select(-starts_with("GCC")) %>% 
  select(-AREASQKM16) %>% 
  mutate(STE_CODE16 = as.numeric(as.character(STE_CODE16))) %>% 
  mutate(SA2_MAIN16 = as.numeric(as.character(SA2_MAIN16))) %>% 
  mutate(SA2_5DIG16 = as.numeric(as.character(SA2_5DIG16)))

# SA2 %<>% left_join(select(m0$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m0 = mean))
SA2 %<>% left_join(select(m1$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m1 = mean))
SA2 %<>% left_join(select(m2$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m2 = mean))
SA2 %<>% left_join(select(m3$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m3 = mean))
SA2 %<>% left_join(select(m4$summary.random$SA2_MAIN16, ID, mean) %>% rename(SA2_MAIN16 = ID, exp_m4 = mean)) %>%
  mutate(exp_m1 = exp(exp_m1),
         exp_m2 = exp(exp_m2),
         exp_m3 = exp(exp_m3),
         exp_m4 = exp(exp_m4))

tm_shape(SA2) + 
  tm_fill(col = "exp_m4")
