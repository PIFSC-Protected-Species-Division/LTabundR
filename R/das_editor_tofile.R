#' Edit `DAS` files and save edited versions
#'
#' This function is basically a wrapper for the function `das_editor()`, but this function saves the edited DAS data to new files.
#' It takes a set of `DAS` files and a set of editing instructions (provided as a list -- see `das_editor()` documentation),
#' applies the edits to each `DAS` file, and saves new versions of the DAS file. It does *NOT* replace the original DAS file.

#' @param das_file A vector of `DAS` data filename(s).
#' @param edits A list of sublists, each being instructions for a staged edit. See `?das_editor()` for details.
#' @param suffix The suffix to append to `DAS` files to distiguish the original file from the new edited file.
#' @param verbose Boolean. Printed updates to the Console?
#'
#' @return At the end of the process, the function returns a vector of `DAS` filenames that should be passed to processing functions such as `process_surveys()` or `das_load()`.
#' @export
#'
das_editor_tofile <- function(das_file,
                              edits,
                              suffix = '_edited',
                              verbose = TRUE){

  #=============================================================================
  # For debugging -- not run!
  if(FALSE){
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    edits <- list(list(das_file = das_file,
                       type = 'text',
                       rows = 20:30,
                       chars = 20:39,
                       edit = 'N21:47.99 W159:45.91'))
    suffix = '_edited'
  }
  #=============================================================================
  #=============================================================================

  das_file_new <- das_file
  #files_to_delete <- c()
  if(!is.null(edits)){
    message('\nProcessing edits ============================================')

    (files_to_edit <- lapply(edits,'[[',1)[[1]])
    # subset to edits that pertain to the das files being processed
    (edits_to_keep <- which(files_to_edit %in% das_file_new))

    if(length(edits_to_keep) > 0){

      # Make edits
      if(verbose){message('--- applying edits...')}
      das_edited <- das_editor(edits)
      das_edited <- das_edited$das
      length(das_edited)

      # Loop through each edited file and save an edited version to file
      i=1
      for(i in 1:length(das_edited)){
        dasi <- das_edited[[i]]
        dasi %>% head
        (das_file_index <- which(das_file_new == dasi$das_file))
        # Make sure this edited das file is actually going to be used in this process_surveys() call
        if(length(das_file_index)>0){
          (fnami <- das_file_new[das_file_index])
          (fnami <- gsub('.das','',fnami))
          (fnami <- gsub('.DAS','',fnami))
          (fnami <- paste0(fnami, suffix, '.das'))

          # Save tmp version
          if(verbose){message('--- writing edited DAS data to file: ', fnami)}

          (das_tmp <- dasi$das) %>% nrow
          write.table(das_tmp, file = fnami, append = FALSE, sep = " ", dec = ".",
                      row.names = FALSE, col.names = FALSE, quote=FALSE)

          # Replace filename in das_file_new with this modified version
          das_file_new[das_file_index] <- fnami

          # Add this replacement file to the vector of files that need deleting
          #(files_to_delete <- c(files_to_delete, fnami))
        }
      } # loop through each edited das file as a result of these edits
      message('\n')
    } # end of if there are actually edits to apply
  } # end if edits isnt NULL

  if(verbose){message('--- finished applying edits!')}
  if(verbose){message('--- returning new set of DAS filenames to process.')}
  return(das_file_new)
}
