## code to prepare `cruz_proc` dataset goes here
# cruz_proc.RData

#library(devtools)
#document()
#load_all()

data(example_settings)
example_settings
example_settings$cohort[[1]]
das_file <- 'data-raw/data/HICEASwinter2020.das'

#example_settings$survey$interpolate <- 120
#das <- das_load(das_file)
#plot(das$Lon, das$Lat)
#nrow(das)
#dasi <- das_interpolate(das, 120, TRUE)
#nrow(dasi)
#plot(dasi$Lon, dasi$Lat, type='l')
#process_strata(dasi, example_settings)

#example_settings$cohorts[[1]]$beaufort_range
#example_settings$cohorts[[1]]$beaufort_range <- 0:4
#example_settings$cohorts[[1]]

cruz_proc <- process_surveys(das_file,
                             settings = example_settings,
                             process_sightings = TRUE,
                             process_subgroups = TRUE,
                             save_local = FALSE)

#cruz_proc$settings$cohorts[[1]]
#cruz_demo$cohorts$default$das %>% group_by(use) %>% summarize(maxb = max(Bft,na.rm=TRUE))
#cruz_demo$cohorts$default$segments %>% group_by(use) %>% summarize(maxb=max(avgBft, na.rm=TRUE))
#cruz_proc$cohorts$default$sightings %>% group_by(included) %>% summarize(maxb=max(Bft, na.rm=TRUE))

if(FALSE){
  # Test cruz object with downstream functions
cruz_structure(cruz_proc)
#cruz_explorer(cruz_proc)
#summarize_effort(cruz_proc)

cruz_proc$cohorts$default$subgroups$sightings
cruz_proc$cohorts$default$subgroups$subgroups %>%
  filter(SightNo == 102)
}

example_cruz <- cruz_proc
usethis::use_data(example_cruz, overwrite = TRUE)


