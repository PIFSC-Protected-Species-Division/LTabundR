## code to prepare `coastline` dataset goes here

library(sf)
coastline <- st_read("data-raw/data/shp/ne_10m_coastline", quiet=TRUE)

usethis::use_data(coastline, overwrite = TRUE)
