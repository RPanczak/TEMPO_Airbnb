library(cartogram)

temp <- left_join(SA2, filter(airbnb_sa2, reporting_month == as.Date("2017-10-01")))
st_write(temp, "data/airdna/clean/carto/input_sa2.shp")

carto_sa_2 <- cartogram_cont(temp, "cumulative", itermax = 10)
carto_sa_2_dorling <- cartogram_dorling(temp, "cumulative")


temp <- left_join(SA1, filter(airbnb_sa1, reporting_month == as.Date("2017-10-01")))
st_write(temp, "data/airdna/clean/carto/input_sa1.shp")

carto_sa_1 <- cartogram_cont(temp, "cumulative", itermax = 10)
carto_sa_2_dorling <- cartogram_dorling(temp, "cumulative")

rm(temp)


tm_shape(carto_sa_2) + tm_polygons("cumulative", n = 10, style = "quantile")

tm_shape(SA2) + tm_borders() +
  tm_shape(carto_sa_2_dorling) + tm_polygons("cumulative", n = 10, style = "quantile")
