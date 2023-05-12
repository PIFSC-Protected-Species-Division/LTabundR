## code to prepare `ships` dataset goes here

ships <- read.csv('data-raw/data/ships.csv')
ships
tail(ships)
usethis::use_data(ships, overwrite = TRUE)
