library(sf)
library(dplyr)
library(terra)

provs = sf::read_sf("data-raw/shp/bou2_4p_ChinaProvince.shp")
poly = dplyr::filter(provs, NAME == "安徽省")
vect = vect(poly)
shp <- sf::as_Spatial(poly)
sp_layout <- list("sp.lines", shp, lwd = 0.5, first = FALSE)
