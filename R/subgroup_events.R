#' Find subgroup size estimates within a DAS file
#'
#' This is an internal function typically not called by a user directly.
#' It is called in `process_subgroups()`, which is an internal function called in `process_surveys()`.
#'
#' @param das A dataframe of `DAS` data, formatted by the `LTabundR` function `load_das()`.
#' @param species_filter Species code(s), as a character vector,
#' if you only want subgroup data for certain species.
#' The default is for false killer whales, *Pseudorca crassidens*, for whom the subgroup
#' data collection protocol was developed (see [Bradford et al. 2018](https://www.int-res.com/abstracts/esr/v36/p297-313/]).
#'
#' @return A `data.frame` in which each row is a single school size estimate for a single subgroup within a single phase of a single sighting
#' (effectively, the 'raw' data for subgroups within the `das` data).
#'
#' @export
#' @import dplyr
#' @import tidyr
#'
subgroup_events <- function(das,
                            species_filter = '033'){
  if(FALSE){ # debugging only  -- not run! =====================================
    data(example_settings)
    settings <- example_settings
    das_file <- "https://raw.githubusercontent.com/ericmkeen/capstone/master/HICEASwinter2020.das"
    cruz <- process_surveys(das_file)
    das <- cruz$cohorts$all$das
    das <- cruz$cohorts$default$das
    species_filter = '033'
  } # ==========================================================================

  # Find DAS rows with event G
  gi <- which(das$Event == 'G')
  length(gi)

  # debugging
  #gi[1]
  #das[183583:(183583 + 30),c(1,2,25:36)]

  # Stage results object in case there are no subgroup rows
  mr <- data.frame()

  # If there are subgroup rows ...
  if(length(gi)>0){
    i <- 1 # gi[1] # for debugging

    # Loop through each subgroup row
    for(i in 1:length(gi)){
      dasg <- das[gi[i],] ; dasg   # G event details
      dasa <- das[gi[i]+1,] ; dasa   # A event details

      # Compile data.frame
      mri_core <- data.frame(Cruise = dasg$Cruise,
                             ship = dasg$ship,
                             Date = substr(dasg$DateTime,1,10),
                             DateTime = dasg$DateTime,
                             Lat = dasg$Lat,
                             Lon = dasg$Lon,
                             OnEffort = dasg$OnEffort,
                             EffType = dasg$EffType,
                             Bft = dasg$Bft,
                             SwellHght = dasg$SwellHght,
                             RainFog = dasg$RainFog,
                             HorizSun = dasg$HorizSun,
                             VertSun = dasg$VertSun,
                             Glare = dasg$Glare,
                             Vis = dasg$Vis,
                             ObsL = dasg$ObsL,
                             Rec = dasg$Rec,
                             ObsR = dasg$ObsR,
                             ObsInd = dasg$ObsInd,
                             SightNo = dasg$Data1,
                             Obs_Sight = dasg$Data3,
                             Species = dasa$Data5,
                             Line = dasg$line_num,
                             SubGrp = dasg$Data2,
                             Event = NA,
                             GSBest = NA, #dasg$Data4 %>% as.numeric,
                             GSH = NA,
                             GSL = NA,
                             Angle = dasg$Data5 %>% as.numeric,
                             RadDist = dasg$Data7 %>% as.numeric,
                             seg_id = dasg$seg_id,
                             #use = dasg$use,
                             dasg[,grep('stratum',names(dasg))])
      mri_core

      # Check to see if the observer is standard
      mri_core <-
        mri_core %>%
        rowwise() %>%
        mutate(ObsStd = ifelse(Obs_Sight %in% c(ObsL, Rec, ObsR), TRUE, FALSE)) %>%
        ungroup() %>%
        as.data.frame
      mri_core

      # Get group size estimates
      obsi <- 1
      for(obsi in 1:8){
        #daso <- dasg[gi[i]+(obsi + 1),] ; daso
        daso <- das[gi[i]+(obsi + 1),] ; daso
        suppressWarnings( eventi <- as.numeric(as.character(daso$Event)) )
        if(!is.na(eventi)){
          if(eventi == obsi){
            mri <- mri_core
            mri$Event <- obsi
            mri$Obs <- daso$Data1 %>% as.character
            mri$GSBest <- daso$Data2 %>% as.numeric
            mri$GSH <- daso$Data3 %>% as.numeric
            mri$GSL <- daso$Data4 %>% as.numeric
            mri
            mr <- rbind(mr,mri)
          }
        }
      }
    }
    mr

    # Format angle and distance
    mr$Angle[mr$Angle > 180] <- 360 - mr$Angle[mr$Angle > 180]
    mr$RadDist <- mr$RadDist/0.53996
    mr$PerpDist <- mr$RadDist*sin(mr$Angle*pi/180)

    # Filter to species?
    nrow(mr)
    if(!is.null(species_filter)){
      mr <- mr %>% dplyr::filter(gsub(' ','',Species) == species_filter)
    }
    nrow(mr)

    # Add foolproof subgroup identifier
    if(nrow(mr)>0){
      mr <- mr %>%
        dplyr::mutate(sgid = paste(Cruise,
                                   gsub('-','',substr(DateTime,1,10)),
                                   SightNo,
                                   SubGrp,
                                   sep='-'))

      # Uniique ID for each date-sighting
      mr$sitid <- paste0(mr$Cruise,'-',mr$Date,'-',mr$SightNo) ; mr
    }

    tail(mr)
  }

  #mr %>% head # review for debugging

  return(mr)
}
