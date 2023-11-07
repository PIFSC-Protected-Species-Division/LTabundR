#' Adjust the timestamp within a `DAS` file
#'
#' This function is typically called within `LTabundR::das_inspector()` as a way of
#' staging an edit that adjusts the timestamps of certain rows. Not typically called directly by a user.
#'
#' @param das A row of `DAS` data. It may already be cropped to the characters
#' corresponding to timestamp, date, lat, and long (characters 6 - 40).
#'
#' @param tz_adjust One of two options: (1) a single numeric value indicating
#' the hours to add to or subtract from the date-time in each `DAS` row, or
#' (2) the character string `"from utc"`, which will indicate that the `DAS` timestamps
#' are in UTC and need to be converted to local time. To do this, the latitude and longitude
#' of each row will be used to determine the timezone of the event, then adjust the
#' timestamp from UTC accordingly. This process can take several minutes.
#'
#' @return A two-slot list: `[[1]] dt` returns the revised date-time characters.
#' `[[2]] das` returns the revised row in full (if a full row was originally provided).
#'
#' @export
#' @import lubridate
#' @import dplyr
#'
das_time <- function(das,
                     tz_adjust='from utc'){

  #=============================================================================
  if(FALSE){
    tz_adjust <- -1
    tz_adjust <- 'from utc'

    # Providing cropped strings
    das_file <- "data-raw/data/HICEASwinter2020.das"
    das <- das_readtext(das_file)
    (das <- substr(das$das,6,40)[1])
    das

    # Providing a full row
    das_file <- "data-raw/data/HICEASwinter2020.das"
    das <- das_readtext(das_file)
    (das <- das$das[1])

    # 1004 issues
    das_file = '../LTAvignette/data/surveys/CenPac1986-2020_Final_alb.das'
    das <- das_readtext(das_file)
    (dasi <- das$das[435620:435630])
    #(das <- substr(das,6,40))
    (das <- dasi[5]) # no time in this row
    (das <- dasi[4]) # yes time in this row
    das # full row
    tz_adjust = 10

    # Try it
    das_time(das, tz_adjust = 10)$dt
    das_time(das, tz_adjust = 10)$das
    das_time(das, tz_adjust = 'from utc')$dt

    das_inspector(das_file)
  }
  #=============================================================================

  suppressWarnings({

    # Stage results
    (mr <- data.frame(raw = das))

    # Handle event in which input is a full row of DAS data
    nchar(das)
    das_sub <- das
    if(nchar(das) > 34){
      das_sub <- substr(das,6, 40)
    }
    das_sub
    (mr$das = das_sub)
    (mr$dt_adj_das <- das_sub)
    (mr$das_revised <- das)

    # Check here whether this is an auxiliary line
    nchar(gsub(' ','',das_sub)) > 2
    if(nchar(gsub(' ','',das_sub)) > 2){

      # This is a DAS row with timestamps...

    # Format datetime
    (mr$dtraw <- substr(das_sub,1,13))
    (mo <- substr(das_sub, 8, 9))
    (dd <- substr(das_sub, 10, 11))
    (yy <- substr(das_sub, 12, 13))
    (hh <- substr(das_sub, 1, 2))
    (mm <- substr(das_sub, 3, 4))
    (ss <- stringr::str_pad(gsub(' ','',substr(das_sub, 5, 6)), width=2, side='right', pad='0'))
    (yypre <- ifelse(yy < 80, '20','19'))
    (dt <- paste0(yypre, yy, '-', mo,'-', dd,' ',hh,':', mm, ':', ss))
    mr$dt <- dt
    mr$dt_utc <- lubridate::as_datetime(dt, tz='UTC')

    # Apply time adjustment
    tz_adjust
    tz_adjust %>% class
    if(is.numeric(tz_adjust)){
      mr$dt_adj <- mr$dt_utc + lubridate::hours(tz_adjust)
    }else{
      # this is the UTC adjustment scenario

      # Format coordinates
      (lats <- substr(das_sub, 15,23))
      (hh <- ifelse(substr(lats, 1,1)=='N', 1, -1))
      (dd <- substr(lats, 2,3))
      (mm <- substr(lats, 5,9))
      (lats <- (hh*as.numeric(dd)) + (as.numeric(mm)/60))
      mr$lat <- lats

      (lons <- substr(das_sub, 25,34))
      (hh <- ifelse(substr(lons, 1,1)=='E', 1, -1))
      (dd <- substr(lons, 2,4))
      (mm <- substr(lons, 6,10))
      (lons <- (hh*as.numeric(dd)) + (as.numeric(mm)/60))
      mr$lon <- lons

      # Look up actual timezone
      (mr$tz_actual <- lutz::tz_lookup_coords(lats, lons, method = "accurate"))

      head(mr)
      mr$dt_adj <- mr$offset_actual <- NA
      if(!is.na(mr$tz_actual)){
        (locali <- lubridate::force_tz(mr$dt_utc,
                                       tzone = mr$tz_actual))
        (offseti <- mr$dt_utc - locali)
        (locali <- mr$dt_utc + lubridate::hours(offseti))
        mr$dt_adj <- as.character(locali)
        mr$gmt_offset <- as.numeric(offseti)
      }
    }

    # Now re-format the adjusted time for DAS
    head(mr)
    (mr$dt_adj_char <- as.character(mr$dt_adj))
    (dti <- gsub('-','', gsub(':', '', gsub(' ','',mr$dt_adj_char))))
    (dti <- paste0(substr(dti, 9, 14),
                   ' ',
                   substr(dti, 5,6),
                   substr(dti, 7,8),
                   substr(dti,3,4),
                   ' ',
                   substr(das_sub, 15,34))) %>% head
    dti
    mr$dt_adj_das <- dti

    # Handle non-time rows (return dt characters to original content)
    if(is.na(mr$dt_utc)){
      mr$dt_adj_das <- mr$dtraw
    }

    # Now update DAS rows
    mr$das_revised <- NA
    if(! identical(mr$raw, mr$das)){
      mr$das_revised <- paste0(substr(mr$raw,1,19),
                             mr$dt_adj_das,
                             substr(mr$raw,41, nchar(mr$raw)))
    }

    }

    mr
    returni <- list(dt = mr$dt_adj_das,
                    das = mr$das_revised)
    returni
  })
  return(returni)
}
