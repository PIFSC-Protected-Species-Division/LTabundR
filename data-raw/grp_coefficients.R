## code to prepare `grp_coefficients` dataset goes here

# Read in dat
suppressMessages({
  dat <- readr::read_delim('data-raw/data/z/Coeff.dat',delim='\t',col_names=FALSE)
})

dat

# save raw version
#dat_raw <- dat
#dat <- dat_raw

dat <- dat$X1  # make into character vector
#dat <- dat[-1] # remove first line
dat <- gsub('\t',' ',dat)  # replace tabs with spaces
dat <- strsplit(dat," ") # split by spaces
dat <- lapply(dat,function(x){x[which(x != '')]}) # remove blank spaces
dat

df <- data.frame()
length(dat)
i=1
for(i in 1:length(dat)){
  dati <- dat[[i]]
  dati <- dati %>% data.frame %>% t
  df <- rbind(df,dati)
}
df
nrow(df)
ncol(df)

c(das$ObsL, das$ObsR, das$ObsInd) %>% unique
# 126, 227, 197, 125, 238, 307, 099

names(df) <- c('obs', 'n', 'var','min','max','w_best','w_high','w_low','model_1',
               'b0','b1',#'b2',
               'b1987','b1988','b1989','b1990','b1992','b1993','b1998','b1999','b2000','b20XX')
df
length(names(df))

group_size_coefficients <- df

usethis::use_data(group_size_coefficients, overwrite = TRUE)
