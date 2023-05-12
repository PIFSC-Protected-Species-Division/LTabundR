################################################################################
# Convert from DAT to CSV
################################################################################

#subdir <- 'MOPS1986+1987+1988+1989+1990/'
#subdir <- 'PODS1992+1993/'
#subdir <- 'Pre1986/'
#subdir <- 'STAR1998+1999+2000+2003+2006/'

# Stage results
maindir <- 'data/ETPStudyAreaCoordinates/'
results_path <- paste0(maindir,'csv/',subdir) ; results_path
if(! dir.exists(results_path)){dir.create(results_path)}

# Loop through each DAT file
data_dir <- paste0(maindir,'dat/',subdir) ; data_dir
lf <- list.files(data_dir)
lf_full <- paste0(data_dir,lf) ; lf_full
i=11
for(i in 1:length(lf)){
  lfi <- lf_full[i] ; lfi
  
  # Read in dat
  suppressMessages(dat <- readr::read_delim(lfi,delim='&&&',col_names=FALSE))
  dat
  dat_raw <- dat
  
  # Prep for loop
  dat <- dat$X1  # make into character vector
  dat <- gsub('\t',' ',dat)  # replace tabs with spaces
  dat <- strsplit(dat," ") # split by spaces
  dat <- lapply(dat,function(x){x[which(x != '')]}) # remove blank spaces
  dat
  head(dat)
  
  # Loop through each row
  df <- data.frame()
  j=1
  for(j in 1:length(dat)){
    datj <- dat[[j]]
    datj <- strsplit(datj,',') %>% unlist
    datj
    
    X1 <- X2 <- X3 <- NA
    
    #if(length(datj)==1){
    #  dat_split <- strsplit(datj,',')[[1]]
    #  if(length(dat_split)>=1){X1 <- dat_split[1]}
    #  if(length(dat_split)>=2){X2 <- dat_split[2]}
    #  if(length(dat_split)>=3){X3 <- paste(dat_split[3:length(dat_split)],collapse=' ')}
    #}else{
    if(length(datj)>1){X1 <- gsub(',','',datj[1])}
    if(length(datj)>=2){X2 <- gsub(',','',datj[2])}
    if(length(datj)>=3){X3 <- paste(gsub(',','',datj[3:length(datj)]),collapse=' ')}
    #}
    
    new_j <- data.frame(X1,X2,X3)
    new_j
    df <- rbind(df, new_j)
  }
  
  names(df) <- c('Lat','Lon','comment')
  df
  
  # Save to file
  fn <- gsub('dat','csv',lf[i])
  fn <- gsub('DAT','csv',fn)
  fn <- paste0(results_path,fn)
  fn
  
  write.csv(df,file=fn,quote=FALSE,row.names=FALSE)
  message(fn)
}

################################################################################
# Map each polygon
################################################################################

subdir <- 'MOPS1986+1987+1988+1989+1990/'
#subdir <- 'PODS1992+1993/'
#subdir <- 'Pre1986/'
#subdir <- 'STAR1998+1999+2000+2003+2006/'

# Stage results
maindir <- 'data/ETPStudyAreaCoordinates/'
results_path <- paste0(maindir,'map/',subdir) ; results_path
if(! dir.exists(results_path)){dir.create(results_path)}

# Loop through each DAT file
data_dir <- paste0(maindir,'csv/',subdir) ; data_dir
lf <- list.files(data_dir)
lf_full <- paste0(data_dir,lf) ; lf_full
i=1
for(i in 1:length(lf)){
  lfi <- lf_full[i]
  coords <- read.csv(lfi)
  #names(coords) <- c('Lat','Lon')
  head(coords)
  
  coords
  coords <- coords[complete.cases(coords[,1:2]),]
  
  # Prep map file
  fn <- gsub('csv','pdf',lf[i])
  fn <- paste0(results_path,fn)
  fn
  
  # Build map
  m <- base_map('etp')
  sp.lines <- points_to_line(data = coords,
                             long = "Lon",
                             lat = "Lat")
  m <- m + tm_shape(sp.lines) + tm_lines(col='firebrick',lwd=2)
  
  # Save to file
  tmap_save(m,fn,width=10,height=7)
  
  message(fn)
}



