# R here
library(INLA)
m1 <- inla(formula=y~1,data=list(y=1:9))
saveRDS(m1, file = "m1.rds")
quit(save = "no")