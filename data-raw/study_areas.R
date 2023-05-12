## code to prepare `study_areas` dataset goes here

study_cnp <- read.csv('data-raw/data/study_areas/CNPStudy.csv')
study_cnp
usethis::use_data(study_cnp, overwrite = TRUE)
