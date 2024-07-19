#' Coerce eastern longitudes to western (i.e., less than - 180)
#'
#' This can be useful for plotting datasets that cross the international date line (IDL).
#' (Designed as a behind-the-scences function that most users wont' use, but making it available just in case.)
#'
#' @param df Dataframe of coordinates with columns `Lon` and `Lat`.
#'
#' @return Corrected dataframe (all coordinates between 0 and less than -180).
#' @export
#'
idl_coerce <- function(df){

  if(FALSE){
    data(eez)
    df <- eez
    df <- sfheaders::sf_to_df(df)
    df$Lon <- df$x
    df$Lat <- df$y
    #sfheaders::sfc_multipolygon()
  }

  coords <- df

  # Test to see if this polygon crosses the IDL
  (idl <- any(c(any(coords$Lon < -180),
                all(c(any(coords$Lon < 0), any(coords$Lon >= 0))))))

  #idl <- TRUE
  if(idl){
    #plot(coords$Lat ~ coords$Lon)
    (wests <- which(coords$Lon >= -180 & coords$Lon < 0))
    (easts <- which(coords$Lon >= 0))
    (east_coords <- coords$Lon[easts])
    (east_coords <- -180 - (180 - east_coords))
    coords$Lon[easts] <- east_coords
    #plot(coords$Lat ~ coords$Lon)
  }

  return(coords)
}
