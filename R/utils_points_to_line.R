#' Convert coordinates to a `sf` line
#'
#' This is an internal function typically not called by a user directly.
#'
#' @param data The dataframe containing columns with coordinates.
#' @param long A character vector with the name of the column with longitude coordinates.
#' @param lat A character vector with the name of the column with latitude coordinates.
#' @param id_field If the dataframe contains data for multiple lines, specify the
#' name of the column (as a character vector) that indicates which line each point belongs to.
#' @param sort_field If the dataframe is not arranged in the sequence you wish, specify the
#' name of the column (as a character vector) you want to arrange by before creating the line.
#'
#' @return A `sp::SpatialLines` object created from `sp::spRbind()`.
#'
#' @export
#'
points_to_line <- function(data,
                           long,
                           lat,
                           id_field = NULL,
                           sort_field = NULL) {

  #=============================================================================
  # For debugging only -- not run!
  if(FALSE){
    data(example_cruz)
    effi <- example_cruz$cohorts$default$das
    names(effi)
    data <- effi
    long = 'Lon'
    lat = 'Lat'
    id_field = NULL
    sort_field = NULL
  }
  #=============================================================================

  # Convert to SpatialPointsDataFrame
  sp::coordinates(data) <- c(long, lat)

  # Arrange the data, if needed, before mapping
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }

  # If there is only one path...
  if (is.null(id_field)) {
    lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(data)), "id")))
    return(lines)

    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {

    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])

    # Save as spatial lines
    sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "line1")))

    # Loop through each spatial line
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)))
      sp_lines <- sp::spRbind(sp_lines, l)
    }

    return(sp_lines)
  }
}

