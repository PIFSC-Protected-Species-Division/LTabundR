## code to prepare `land` dataset goes here

library(sf)
land <- st_read("data-raw/data/shp/ne_10m_land", quiet=TRUE)

usethis::use_data(land, overwrite = TRUE)
