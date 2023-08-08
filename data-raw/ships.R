## code to prepare `ships` dataset goes here

ships <- read.csv('data-raw/data/ships.csv')
ships
ships <- ships[-55,]
ships
usethis::use_data(ships, overwrite = TRUE)
