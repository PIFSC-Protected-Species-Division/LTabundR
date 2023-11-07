#' Apply edits to a `DAS` file...
#'
#' ...without modifying the original data. Edits are provided as a list, which the
#' user can store as a `RData` object or simply prepare with an `R` script. This
#' function loops through each edit and applies them to the respective `DAS` file.
#' This allows survey data to be modified reproducibly before being processed
#' with `LTabundR::process_surveys()`, without touching the original `DAS` data files or
#' requiring analysts to duplicate files and make one-off modifications manually.
#'
#' @param edits A list of sublists, each being a staged edit. Each sublist requires the following named slots:
#' \itemize{
#' \item `das_file`: Filepath to a `DAS` file.
#' \item `type`: The type of edit. Currently accepted options are:
#' \itemize{
#' \item `"text"`: the `edit` will be interpreted verbatim as text that will replace the specified data.
#' Both `rows` and `characters` must be supplied, as well as the `edit` itself.
#' \item `"function"`: the `edit` will be evaluated as a function that is applied to each row of specified data.
#' Both `rows` and `characters` must be supplied, as well as `edit.`
#' For example, `"tolower"` will convert the specified data to all lower case.
#' Another example: `"function(x){LTabundR::das_time(x, -10)}"`
#' will subtract 10 hours from the data if your specified `chars` is the date-timestamp for a row.
#' \item `"move"`: the `rows` will be deleted from their current location and pasted
#' immediately below the row number specified by `edit`.
#' The moved rows will be given the same date, time, latitude, and longitude, as the `edit` row.
#' Only `rows` and `edits` need to be supplied.
#' \item `"copy"`: the `rows` will be copied from their current location and pasted
#' immediately below the row number specified by `edit`.
#' The paste rows will be given the same date, time, latitude, and longitude, as the `edit` row.
#' Only `rows` and `edits` need to be supplied.
#' \item `"insert"`: the text provided in `edit` will be inserted verbatim immediately below
#' the first of the `rows` provided. Only `rows` and `edits` needs to be supplied.
#' \item `"delete"`: the `rows` will be deleted. Only `rows` needs to be supplied.
#' }
#' \item `rows`: A numeric vector of the rows to be edited.
#' \item `chars`: The character indices to be edited; e.g., `"1:3"` specifies that only the first three characters in each of `rows` will be edited.
#' \item `edit`: The actual edit, provided as a vector whose format depends on the `type` chosen above. See examples.
#' }
#' This `edits` list of lists can be as long as necessary, and it can contain as edits for as many `DAS` files as needed.
#' The `das` table that is returned by the function will concatenate all `das` files represented by the edits into a single table.
#'
#' @return A list with two named slots:
#' \itemize{
#' \item `das`: A `data.frame` containing the modified `DAS` data (with one column: `$das`).
#' \item `log`: The `edits` input, with each sublist augmented with a `results` slot that shows the results of the edit.
#' }
#' @export
#' @import dplyr
#' @import tidyr
#'
das_editor <- function(edits){

  if(FALSE){ # for debugging only -- not run! ==================================
    # Fake edits - testing each separately
    das_file <- "data-raw/data/HICEASwinter2020.das"
    das <- das_readtext(das_file)

    # Verbatim text replacement ====================
    (edits <- list(list(das_file = das_file, type = 'text',
                       rows = 10:15, chars = 20:39, edit = 'lat, lon')))
    das$das[9:16]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:16]

    # Function text replacement  ====================
    (edits <- list(list(das_file = das_file, type = 'function',
                        rows = 10:15, chars = 20:39, edit = 'tolower')))
    das$das[9:16]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:16]

    # Time adjustment
    (edits <- list(list(das_file = das_file, type = 'function',
                        rows = 1:10, chars = 6:40,
                        edit = 'function(x){das_time(x)$dt}')))
    das$das[1:10]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[1:10]

    # Move ==========================================
    (edits <- list(list(das_file = das_file, type = 'move',
                        rows = 10, chars = NULL, edit = 15)))
    das$das[9:16]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:16]

    # Copy & paste ==================================
    (edits <- list(list(das_file = das_file, type = 'copy',
                        rows = 10, chars = NULL, edit = 15)))
    das$das[9:17]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:17]

    # Delete ==================================
    (edits <- list(list(das_file = das_file, type = 'delete',
                        rows = 10, chars = NULL, edit = NULL)))
    das$das[9:16]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:16]

    # Insert ==================================
    (edits <- list(list(das_file = das_file, type = 'insert',
                        rows = 10, chars = NULL,
                        edit = "SEQ* HHMMSS MMDDYY NDG:MI.NT WDEG:MI.NT")))
    das$das[9:16]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[9:16]

    # Actual edits ============================
    das_file <- '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das'
    das <- das_readtext(das_file)

    edits <- list(
      # Cruise 1607, 1997-04-27 07:16am sighting (spp 037), currently missing valid observers and sighting conditions.
      # Problem is a Resume event without observer / weather updates a few lines prior.
      # This edit copies the most reent Observer and Weather lines and pastes them just below that R event.
      list(das_file = '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das',
           type = 'copy',
           rows = c(129982, 129985),
           chars = NULL,
           edit = 129987),
      # Cruise 1607, 1997-04-15 1230 sighting (spp 022), currently missing valid observers.
      # Problem is a Resume event without observer updates a few lines prior.
      # This edit copies the most recent Observer line and pastes them just below that R event.
      list(das_file = '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das',
           type = 'copy',
           rows = 128111,
           chars = NULL,
           edit = 128118)
    )

    # First edit
    das$das[129981:129992]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[129982:129995]

    # Second edit
    das$das[128111:128121]
    dase <- das_editor(edits)
    dase$das[[1]]$das$das[128111:128123]

  } # end debugging ============================================================

  # Stage master results
  MR <- list()
  edit_log <- list()

  # Get unique das files represented within staged edits
  (dass <- lapply(edits,'[[',1) %>% unlist)
  (udas <- unique(dass))

  # Loop through each unique das file for which there are staged edits
  ui = 1
  for(ui in 1:length(udas)){
    (dasu <- udas[ui])
    message('Applying edits to DAS file: ',dasu)

    # Read in das file
    dasi <- das_readtext(dasu)

    # Stage revised dataframe
    revised_das <- dasi

    # Subset list of edits to those for this file only
    editdasu <- edits[which(dass == udas[ui])]
    editdasu

    # Sort these edits according to type
    (types <- lapply(editdasu, '[[',2) %>% unlist)

    # Text replacements ====================================================
    (eds <- which(types %in% c('text', 'function')))
    if(length(eds)>0){
      (edi <- editdasu[eds])
      message('--- verbatim/function text replacements ...')

      ei = 1 # Loop through each edit
      for(ei in 1:length(edi)){
        message('--- --- working on edit ', ei,' ...')
        (editi <- edi[[ei]])
        editi
        (rows <- editi$rows)
        (chars <- editi$chars)
        (dasrows <- revised_das[rows,])

        # Content to keep before substitutions
        if(min(chars)==1){ keep_pre <- ''
        }else{ keep_pre <- substr(dasrows,1, (min(chars)-1)) }
        keep_pre

        # Text to be substituted
        (substituted <- substr(dasrows,min(chars), max(chars)))

        # Substitution content
        (replacement <- editi$edit)
        if(editi$type == 'function'){
          (fun <- eval(parse(text=replacement)))
          #fun <- function(x){das_time(x, tz_adjust=-1)$dt}
          #if(length(substituted)>2){
          #  pb <- txtProgressBar(max=length(substituted))
          #}
          substituted
          replaci <- c()
          #i=1
          for(i in 1:length(substituted)){
            #if(length(substituted)>2){
            #  setTxtProgressBar(pb, i)
            #}
            #(das <- substituted[i])
            #das_time(das, tz_adjust = 10)
            (replaci[i] <- fun(substituted[i]))
          }
          (replacement <- replaci)
          #replacement <- as.character(sapply(substituted, fun))
        }

        # Content to keep after substitutions
        (keep_pos <- substr(dasrows, (max(chars)+1), nchar(dasrows)))

        # Make changes to rows
        (editedrows <- paste0(keep_pre, replacement, keep_pos))

        # Change edited das rows
        revised_das[rows,] <- editedrows

        # Add to edit log
        editi$result <- editedrows
        edit_log[[length(edit_log) + 1]] <- editi
      }
    }

    # Move, copy/paste, insertions and deletions  ====================================
    (eds <- which(types %in% c('move', 'copy','insert','delete')))
    if(length(eds)>0){
      (edi <- editdasu[eds])
      message('--- move, copy/paste, insertion, and deletion events ...')

      revised_das2 <- revised_das
      revised_das2$id <- 1:nrow(revised_das2) # Store original row locations

      ei = 1 # Loop through each edit
      for(ei in 1:length(edi)){
        message('--- --- working on edit ', ei,' ...')
        (editi <- edi[[ei]])
        (typi <- editi$type)

        #====================
        if(typi == 'move'){
          (copied_ids <- revised_das2$id[which(revised_das2$id %in% editi$rows)]) # ids of rows to be copied
          (copied_data <- revised_das2[revised_das2$id %in% copied_ids,])
          (copied_row <- which(revised_das2$id == copied_ids))

          # Execute delete
          revised_das2 <- revised_das2[-copied_row,]

          # Find reference rows
          (ref_id <- revised_das2$id[which(revised_das2$id %in% editi$edit)]) # id of row to paste after
          (ref_row <- which(revised_das2$id %in% editi$edit))
          (ref_data <- revised_das2[revised_das2$id %in% ref_id,])
          (ref_dt <- substr(ref_data$das, 6, 39))

          # Replace copied data timestamps
          (copied_data$das <- paste0(substr(copied_data$das,1,5),
                                     ref_dt,
                                     substr(copied_data$das,40,nchar(copied_data$das))))

          # Execute paste (move)
          revised_das2 <- revised_das2 %>% tibble::add_row(copied_data, .after= ref_row)
          (edited_rows <- revised_das2$das[(ref_row):(ref_row + nrow(copied_data) + 1)])
        }

        #====================
        if(typi == 'copy'){
          (copied_ids <- revised_das2$id[which(revised_das2$id %in% editi$rows)]) # ids of rows to be copied
          (copied_data <- revised_das2[revised_das2$id %in% copied_ids,])

          (ref_id <- revised_das2$id[which(revised_das2$id %in% editi$edit)]) # id of row to paste after
          (ref_row <- which(revised_das2$id %in% editi$edit))
          (ref_data <- revised_das2[revised_das2$id %in% ref_id,])
          (ref_dt <- substr(ref_data$das, 6, 39))

          # Replace copied data timestamps
          (copied_data$das <- paste0(substr(copied_data$das,1,5),
                                    ref_dt,
                                    substr(copied_data$das,40,nchar(copied_data$das))))

          # Execute paste
          revised_das2 <- revised_das2 %>% tibble::add_row(copied_data, .after= ref_row)
          (edited_rows <- revised_das2$das[(ref_row):(ref_row + nrow(copied_data) + 1)])
        }

        #====================
        if(typi == 'insert'){
          (copied_data <- data.frame(das = editi$edit,
                                     id = editi$rows[1] + 1))

          (ref_id <- revised_das2$id[which(revised_das2$id %in% editi$rows)]) # id of row to paste after
          (ref_row <- which(revised_das2$id %in% editi$rows))
          (ref_data <- revised_das2[revised_das2$id %in% ref_id,])

          # Execute insertion
          revised_das2 <- revised_das2 %>% tibble::add_row(copied_data, .after= ref_row)
          (edited_rows <- revised_das2$das[(ref_row):(ref_row + nrow(copied_data) + 1)])
        }

        #====================
        if(typi == 'delete'){
          (ref_id <- revised_das2$id[which(revised_das2$id %in% editi$rows)]) # id of row to paste after
          (ref_row <- which(revised_das2$id %in% editi$rows))
          (ref_data <- revised_das2[revised_das2$id %in% ref_id,])

          # Execute deletion
          revised_das2 <- revised_das2[-ref_row,]
          (edited_rows <- revised_das2$das[(ref_row-1):(ref_row + 1)])
        }

        # Add to edit log
        editi$result <- edited_rows
        edit_log[[length(edit_log) + 1]] <- editi
      }

      # Updated revised das
      revised_das2$id <- NULL
      revised_das <- revised_das2
    }

    # Add this edited das file to the master result
    MR[[length(MR) + 1]] <- list(das_file = dasu,
                                 das = revised_das)

  } # loop to next das file represented

  # Stage final return =========================================================

  return_list <- list(das = MR,
                      log = edit_log)
  return_list

  return(return_list)
}
