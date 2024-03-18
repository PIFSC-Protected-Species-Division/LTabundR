## code to prepare `eez` dataset goes here

library(sf)
eez <- st_read("data-raw/data/shp/USMaritimeLimitsAndBoundariesSHP", quiet=TRUE)
usethis::use_data(eez, overwrite = TRUE)

# Subset eez to polygons of interest for preset region options

# CA OR WA
eez_ccs <- sf::st_geometry(eez[eez$REGION=='Pacific Coast',])
usethis::use_data(eez_ccs, overwrite = TRUE)


# HAWAII
hawaii <- sf::st_geometry(eez[eez$REGION=='Hawaiian Islands',])

# Replace positive coordinates with negative
hawaii
length(hawaii)
newxy <- list()
for(i in 1:length(hawaii)){
  (hawaiii <- hawaii[i])
  (coords <- data.frame(st_coordinates(hawaiii)))
  hist(coords$X)
  (tochange <- which(coords$X > 0))
  if(length(tochange)>0){
    coords$X[tochange] <- -180 - (180 - coords$X[tochange])
    hist(coords$X)
  }
  mx <- coords[,1:2]
  mx %>% head
  mx <- matrix(data=c(mx$X, mx$Y), byrow=FALSE, ncol=2)
  newxy[[length(newxy)+1]] <- mx
}
length(newxy)
newxy[[1]] %>% head

hawaii_new <- st_multilinestring(newxy)
hawaii_new <- st_cast(hawaii_new, to='MULTILINESTRING')
hawaii_new <- sf::st_geometry(hawaii_new)
eez_hawaii <- hawaii_new
eez_hawaii <- hawaii
usethis::use_data(eez_hawaii, overwrite = TRUE)

