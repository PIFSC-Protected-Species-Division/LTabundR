# Barlow 2015 g0 results

library(dplyr)
library(gsheet)

data(g0_results)
g0_results %>% head

url <- 'https://docs.google.com/spreadsheets/d/1mzyGb2qMUkpXb2ChglVMx9ZXRaGpjq0C2-Xzlj6uYvg/edit?usp=sharing'
df <- gsheet2tbl(url)
head(df)

# Note that Cuviers and UNID Mesopp are absolute estimates, not relative estimates

results <- data.frame()
i=1
for(i in 1:nrow(df)){
  (dfi <- df[i,])
  message(dfi$title)
  (resulti <- dfi[,1:6])
  if(nchar(resulti$spp[1])==2){resulti$spp <- paste0('0',resulti$spp)}
  (resulti <- data.frame(resulti, bft = 0:6, Rg0 = NA, Rg0_CV = NA))
  resulti[resulti$bft == 0, 8:9] <- as.numeric(dfi[1,7:8])
  resulti[resulti$bft == 1, 8:9] <- as.numeric(dfi[1,9:10])
  resulti[resulti$bft == 2, 8:9] <- as.numeric(dfi[1,11:12])
  resulti[resulti$bft == 3, 8:9] <- as.numeric(dfi[1,13:14])
  resulti[resulti$bft == 4, 8:9] <- as.numeric(dfi[1,15:16])
  resulti[resulti$bft == 5, 8:9] <- as.numeric(dfi[1,17:18])
  resulti[resulti$bft == 6, 8:9] <- as.numeric(dfi[1,19:20])
  resulti
  results <- rbind(results, resulti)
}

results

barlow_2015 <- results
tail(barlow_2015)
barlow_2015 %>% View
usethis::use_data(barlow_2015, overwrite = TRUE)

