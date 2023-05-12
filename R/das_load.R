#' Read in and process a `DAS` file
#'
#' This is an internal function typically not called by a user directly.
#' It is the first subroutine called within `process_surveys()`.
#' This function is essentially a wrapper for `swfscDAS::das_read()`, which reads in a raw
#' `DAS` file (or a URL to an `DAS` file in an online repository), and `swfscDAS::das_process()`, which formats each row into columns,
#' with survey status, sighting conditions, and observer positions added to each row as new columns.
#'
#' @param das_file Filepath to WinCruz `DAS` file with survey data.
#' Filepath can be absolute or relative. It can also be a URL.
#' @param perform_checks Boolean, with default of `FALSE`; should `swfscDAS::das_check()` be run first, with diagnostics printed to the console?
#' @param print_glimpse Boolean, with default of `FALSE`; print a glimpe (`dplyr::glimpse()`) of the formatted `DAS` dataframe?
#'
#' @return A `data.frame` of the `DAS` file, with fields parsed into columns and with new columns for survey status,
#' sighting conditions, and observer positions.
#'
#' @export
#'
das_load <- function(das_file,
                     perform_checks = FALSE,
                     print_glimpse = FALSE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    #das_load(das_file)
    perform_checks = TRUE
    print_glimpse = TRUE
  }
  #=============================================================================

  # Check for issues using swfscDAS function
  if(perform_checks){
    das_check <- swfscDAS::das_check(das_file, skip = 0, print.cruise.nums = TRUE)
  }

  # Read data using swfscDAS function `das_read`.
  suppressMessages({
    suppressWarnings({
      das_raw <- swfscDAS::das_read(das_file, skip = 0)
    })
  })

  # Process data using swfscDAS function
  df <- swfscDAS::das_process(das_raw)

  if(print_glimpse){
    # Review result using dplyr function
    dplyr::glimpse(df)

    # The number of each event
    table(df$Event)
  }

  return(df)
}
