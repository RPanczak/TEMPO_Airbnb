library("plotKML")
library("sp")
library("spacetime")

data("fmd")
fmd0 <- data.frame(fmd)
coordinates(fmd0) <- c("X", "Y")
proj4string(fmd0) <- CRS("+init=epsg:27700")
fmd_sp <- as(fmd0, "SpatialPoints")
dates <- as.Date("2001-02-18")+fmd0$ReportedDay

fmd_ST <- STIDF(sp = fmd_sp, time = dates, data.frame(ReportedDay=fmd0$ReportedDay))

plotKML(fmd_ST, colour_scale=SAGA_pal[[1]])
