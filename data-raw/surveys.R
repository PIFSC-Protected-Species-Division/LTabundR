## code to prepare `surveys` dataset goes here

# HICEAS 2020
library(swfscDAS)
das_file <- 'data-raw/data/HICEASwinter2020.das'
das_raw <- das_read(das_file, skip = 0)
hiceas2020 <- das_process(das_raw)
usethis::use_data(hiceas2020, overwrite = TRUE)
