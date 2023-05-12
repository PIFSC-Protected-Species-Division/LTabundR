## code to prepare `cruz_proc` dataset goes here
# cruz_proc.RData

#library(devtools)
#document()
#load_all()

data(example_settings)
example_settings
das_file <- 'data-raw/data/HICEASwinter2020.das'

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
