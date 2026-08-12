## code to prepare `grp_coefficients` datasets goes here

################################################################################
# grp_coeff_abund

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

#group_size_coefficients <- df
grp_coeff_abund <- df

usethis::use_data(grp_coeff_abund, overwrite = TRUE)

################################################################################
# create a built-in dataset for gerrodette calibration approach

grp_coeff_gerrodette <- data.frame(
  species = c(
    # Spinner dolphins
    '003','010','011','088','100','101','102','103',
    # Spotted dolphins
    '002','006','090',
    # Striped dolphins
    '013',
    # Common dolphins
    '005','016','017',
    # "Other" (common bottlenose, risso's, short-finned pilot whale, rough-toothed)
    '018', '021', '036', '015'
  ),
  beta = c(
    rep(0.603, times = 8), # spinner
    rep(0.656, times = 3), # spotted
    rep(0.513, times = 1), # striped
    rep(0.757, times = 3), # common
    rep(0.423, times = 4) # other
  ),
  intercept = 0.796,
  floor = 25)

grp_coeff_gerrodette

usethis::use_data(grp_coeff_gerrodette, overwrite = TRUE)

################################################################################
# create grp_ops datasets

# one for abund
data('grp_coeff_abund', package='LTabundR')
grp_ops_abund <- list(method = 'ABUND',
                      coefficients = grp_coeff_abund,
                      floor = 0,
                      intercept = NULL,
                      beta = NULL,
                      beta_mixed = NULL)
grp_ops_abund
usethis::use_data(grp_ops_abund, overwrite = TRUE)

# one for gerrodette
data('grp_coeff_gerrodette', package='LTabundR')
grp_ops_gerrodette <- list(method = 'Gerrodette',
                           coefficients = grp_coeff_gerrodette,
                           floor = 25,
                           intercept = 0.796,
                           beta = 0.626,
                           beta_mixed = NULL)
grp_ops_gerrodette
usethis::use_data(grp_ops_gerrodette, overwrite = TRUE)

################################################################################

#
