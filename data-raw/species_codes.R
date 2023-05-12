## code to prepare `species_codes` dataset goes here

library(dplyr)
library(stringr)


# Match this formatting
species_codes <- read.csv('data-raw/data/species_codes.csv')
head(species_codes)
species_codes %>% names

df <- read.csv('data-raw/data/SpCodes.csv', header=FALSE, stringsAsFactors=FALSE)
df[1,]


common_name1 <- common_name2 <- common_name3 <- common_name4 <- rep('',times=nrow(df))
i=20
for(i in 1:nrow(df)){
  (dfi <- df[i,])

  (splits <- str_split(dfi$V4, ', ')[[1]])
  (cn <- splits[splits != ''])
  common_name1[i] <- cn[1]
  if(length(cn)>=2){ common_name2[i] <- cn[2]}
  if(length(cn)>=3){ common_name3[i] <- cn[3]}
  if(length(cn)>=4){ common_name4[i] <- cn[4]}
}

species_codes <- df[,1:3]
names(species_codes) <- c('code', 'short_name', 'scientific_name')
head(species_codes)
species_codes$common_name1 <- common_name1
species_codes$common_name2 <- common_name2
species_codes$common_name3 <- common_name3
species_codes$common_name4 <- common_name4

head(species_codes)

species_codes %>% View



usethis::use_data(species_codes, overwrite = TRUE)
