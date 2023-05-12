#' Read DAS file without parsing columns
#'
#' In contrast to `swfscDAS::das_read()`, this function reads the DAS text in, saving
#' each line of data to a single row of a one-column `data.frame` (no column parsing).
#' This is useful if you wish to edit and save an altered version of the data using
#' `LTabundR::das_inspector()` and `LTabundR::das_editor()`. This function relies upon
#' the `readtext` package.
#'
#' @param das_file Filepath to `DAS` file.
#'
#' @return A one-column `data.frame` (column name is `das`).
#' @export
#'
das_readtext <- function(das_file){
  suppressWarnings({
    mr <- readtext::readtext(das_file)
  })
  splits <- stringr::str_split(mr$text, '\n')[[1]]
  df <- data.frame(das = splits)
  return(df)
}
