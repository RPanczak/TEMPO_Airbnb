# #################################################
# inla on HPC

# # messaging file
# fileConn <- file("../xxx.log", "w")
# writeLines(paste(Sys.time(), "Job started"), fileConn)

# #################################################
set.seed(12345)
library(INLA)



# #################################################

writeLines(paste(Sys.time(), "Finished"), fileConn)
close(fileConn)
