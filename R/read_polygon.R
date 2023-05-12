#' Read in a DAT file with polygon(s)
#'
#' This is an internal function typically not called by a user directly.
#' This function reads in a `.DAT` file with coordinates for one or more polygons,
#' returning a list.
#'
#' @param dat_file Filepath to a `.DAT` file containing coordinates for one or more polygons.
#'
#' @return A named list:
#' \enumerate{
#' \item `polygons` A dataframe summarizing the polygons contained in the `.DAT` file.
#' It has four columns: `name` (polygon name), `area` (polygon area, if provided),
#' `name_i` (the index of the polygon in the file), and `area_i` (the index of the area provided in the file).
#' \item `coordinates` A list with a slot for each polygon contained within the `.DAT` file. Each polygon is a
#' dataframe with two columns: `Lon` and `Lat`, with coordinates formatted as provided by the `.DAT` file.
#' \item `dat` The raw `DAT` file, as it was read by `R`, returned as a dataframe.
#' }
#' @export
#'
read_polygon <- function(dat_file){

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    # The CNP dat file - local
    dat_file <- 'data-raw/data/StratCNP120BD.dat'
  }
  #=============================================================================

  # Read in dat as a fixed-width space separated file
  suppressMessages(dat <- readr::read_delim(dat_file,delim='\t',col_names=FALSE))
  dat

  # save raw version
  dat_raw <- dat

  dat <- dat$X1  # make into character vector
  dat <- dat[-1] # remove first line
  dat <- gsub('\t',' ',dat)  # replace tabs with spaces
  dat <- strsplit(dat," ") # split by spaces
  dat <- lapply(dat,function(x){x[which(x != '')]}) # remove blank spaces

  dat

  # Figure out which polygons are in this dat file
  suppressWarnings({
    dat_lengths <- unlist(lapply(dat,length)) # get length of each index
    dat_character <- unlist(lapply(dat,function(x){all(is.na(as.numeric(x)))})) # is each index character?
  })
  dat_lengths
  dat_character

  # Use those vectors to figure out which indices have polygon names and areas
  poly_names_i <- which(dat_lengths == 1 & dat_character == TRUE) ; poly_names_i
  poly_areas_i <- which(dat_lengths == 1 & dat_character == FALSE) ; poly_areas_i

  # Summarize in a table
  poly_set <- data.frame(name = unlist(dat[poly_names_i]),
                         area = unlist(dat[poly_areas_i]),
                         name_i = poly_names_i,
                         area_i = poly_areas_i) ; poly_set

  # Inventory which indices are coordinates
  coord_i <- 1:length(dat)
  coord_i <- coord_i[-c(poly_names_i,poly_areas_i)] ; coord_i

  # Create list with coordinates for each polygon
  poly_coords <- list()
  i=1
  for(i in 1:nrow(poly_set)){
    # Loop through each polygon

    # Get starting coordinate index
    i_begin <- poly_set$area_i[i] + 1

    # Get ending coordinate index
    if(i != nrow(poly_set)){
      i_end <- poly_set$name_i[i+1] - 1
    }else{
      i_end <- length(dat)
    }

    # Subset to those coordinates
    coords <- coord_i[which(coord_i == i_begin) : which(coord_i == i_end)]
    coords <- dat[coords]

    # Turn into a dataframe
    coords <- data.frame(Lon = sapply(coords,'[[',2),
                         Lat = sapply(coords,'[[',1))

    # Format longitude
    coords$Lon <- -1*as.numeric(coords$Lon)
    coords

    # Add to list
    poly_coords[[length(poly_coords)+1]] <- coords
  }

  names(poly_coords) <- poly_set$name

  poly_set
  poly_coords

  return_list <- list(polygons = poly_set,
                      coordinates = poly_coords,
                      dat = dat_raw)

  return(return_list)
}
