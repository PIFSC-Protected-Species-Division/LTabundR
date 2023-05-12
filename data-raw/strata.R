## code to prepare `strata` dataset goes here

# settings will accept a list of dataframes.
# If there are multiple, provide them as a list.

# hawaii / cnp =================================================================

strata_cnp <- list()


# Other CNP and HI-EEZ  ========================================================

subdir <- 'data-raw/data/strata/hawaii/'
lf <- list.files(subdir)
lfull <- paste0(subdir,lf)
lfull

strata_cnp$HI_EEZ <- read.csv(lfull[1]) %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))
strata_cnp$OtherCNP <- read.csv(lfull[2]) %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))

head(strata_cnp$HI_EEZ)


# MAIN HAWAIIAN ISLAND ======================================================================

(strati <- read.csv('data-raw/data/new_strata/MainIslandStratum.txt', header = FALSE))
strati <- data.frame(Lon = strati[,2], Lat = strati[,1])
head(strati)
strata_cnp$MHI <- strati


# WHICEAS ======================================================================

(strati <- read.csv('data-raw/data/new_strata/WHICEAS/WHICEASStudyArea.csv'))
strati <- data.frame(Lon = strati$lon, Lat = strati$lat)
head(strati)
strata_cnp$WHICEAS <- strati %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))


# Spotted dolphins =============================================================

(strati <- read.csv('data-raw/data/new_strata/Spotted/SpottedBoundaryAll.csv'))
strati$Island %>% unique

# OU
isli <- strati %>% dplyr::filter(Island == 'OU')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Spotted_OU <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))

# FI
isli <- strati %>% dplyr::filter(Island == 'FI')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Spotted_FI <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))

# BI
isli <- strati %>% dplyr::filter(Island == 'BI')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Spotted_BI <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))


# Bottlenose dolphins =============================================================

(strati <- read.csv('data-raw/data/new_strata/Bottlenose/BottlenoseBoundaryAll.csv'))
strati$Island %>% unique

# KaNi
isli <- strati %>% dplyr::filter(Island == 'KaNi')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Bottlenose_KaNi <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))

# OUFI
isli <- strati %>% dplyr::filter(Island == 'OUFI')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Bottlenose_OUFI <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))

# BI
isli <- strati %>% dplyr::filter(Island == 'BI')
isli <- data.frame(Lon = isli$lon, Lat = isli$lat)
head(isli)
strata_cnp$Bottlenose_BI <- isli %>%
  dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))


# False Killer Whale Strata ====================================================

(strati <- read.csv('data-raw/data/new_strata/NWHINew.txt', header = FALSE))
strati <- data.frame(Lon = strati[,2], Lat = strati[,1])
head(strati)
strata_cnp$NWHI <- strati  %>% dplyr::mutate(Lon = ifelse(Lon > 0, -180 - (180 - Lon), Lon))


#===============================================================================

names(strata_cnp)

# check
lapply(strata_cnp,nrow)

# store data
usethis::use_data(strata_cnp, overwrite = TRUE)


# ca-or-wa / ccs ===============================================================

subdir <- 'data-raw/data/strata/ca-or-wa/'
lf <- list.files(subdir)
lfull <- paste0(subdir,lf)
lfull

strata_ccs <- list()
strata_ccs$CCS <- read.csv(lfull[3])
strata_ccs$Southern_CA <- read.csv(lfull[5])
strata_ccs$Central_CA <- read.csv(lfull[1])
strata_ccs$Nothern_CA <- read.csv(lfull[2])
strata_ccs$OR_WA <- read.csv(lfull[4])

# check
lapply(strata_ccs,nrow)

# store data
usethis::use_data(strata_ccs, overwrite = TRUE)


# ETP ==========================================================================

strata_etp <- list()
list.files('data-raw/data/strata/etp')

# MOP
subname <- 'MOPS'
subdir <- 'data-raw/data/strata/etp/MOPS1986+1987+1988+1989+1990/'
lf <- list.files(subdir, recursive=TRUE)
lfull <- paste0(subdir,lf)
lfull
i=1
for(i in 1:length(lf)){
  lfi <- lf[i] ; lfi
  lfulli <- lfull[i] ; lfulli
  lfmod <-  paste0(subname,'_',gsub('.csv','',lfi)) ; lfmod
  strata_etp[[length(strata_etp)+1]] <- read.csv(lfulli)
  names(strata_etp)[length(strata_etp)] <- lfmod
  #strata_etp
}
names(strata_etp)


# PODS
subname <- 'PODS'
subdir <- 'data-raw/data/strata/etp/PODS1992+1993/'
lf <- list.files(subdir, recursive=TRUE)
lfull <- paste0(subdir,lf)
lfull
i=1
for(i in 1:length(lf)){
  lfi <- lf[i] ; lfi
  lfulli <- lfull[i] ; lfulli
  lfmod <-  paste0(subname,'_',gsub('.csv','',lfi)) ; lfmod
  strata_etp[[length(strata_etp)+1]] <- read.csv(lfulli)
  names(strata_etp)[length(strata_etp)] <- lfmod
  #strata_etp
}
names(strata_etp)


# Pre1986
subname <- 'Pre1986'
subdir <- 'data-raw/data/strata/etp/Pre1986/'
lf <- list.files(subdir, recursive=TRUE)
lfull <- paste0(subdir,lf)
lfull
i=1
for(i in 1:length(lf)){
  lfi <- lf[i] ; lfi
  lfulli <- lfull[i] ; lfulli
  lfmod <-  paste0(subname,'_',gsub('.csv','',lfi)) ; lfmod
  strata_etp[[length(strata_etp)+1]] <- read.csv(lfulli)
  names(strata_etp)[length(strata_etp)] <- lfmod
  #strata_etp
}
names(strata_etp)


# STAR1998+1999+2000+2003+2006
subname <- 'STAR'
subdir <- 'data-raw/data/strata/etp/STAR1998+1999+2000+2003+2006/'
lf <- list.files(subdir, recursive=TRUE)
lfull <- paste0(subdir,lf)
lfull
i=1
for(i in 1:length(lf)){
  lfi <- lf[i] ; lfi
  lfulli <- lfull[i] ; lfulli
  lfmod <-  paste0(subname,'_',gsub('.csv','',lfi)) ; lfmod
  strata_etp[[length(strata_etp)+1]] <- read.csv(lfulli)
  names(strata_etp)[length(strata_etp)] <- lfmod
  #strata_etp
}
names(strata_etp)
lapply(strata_etp,nrow) %>% unlist

# store data
usethis::use_data(strata_etp, overwrite = TRUE)

