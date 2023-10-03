## code to prepare `cruz_proc` dataset goes here
# cruz_proc.RData

#library(devtools)
#document()
#load_all()

data(example_settings)
example_settings
das_file <- 'data-raw/data/HICEASwinter2020.das'

#example_settings$survey$interpolate <- 120
#das <- das_load(das_file)
#plot(das$Lon, das$Lat)
#nrow(das)
#dasi <- das_interpolate(das, 120, TRUE)
#nrow(dasi)
#plot(dasi$Lon, dasi$Lat, type='l')
#process_strata(dasi, example_settings)

cruz_proc <- process_surveys(das_file,
                             settings = example_settings,
                             process_sightings = TRUE,
                             process_subgroups = TRUE,
                             save_local = FALSE)

# Test cruz object with downstream functions
cruz_structure(cruz_proc)
#cruz_explorer(cruz_proc)
#summarize_effort(cruz_proc)

example_cruz <- cruz_proc
usethis::use_data(example_cruz, overwrite = TRUE)


