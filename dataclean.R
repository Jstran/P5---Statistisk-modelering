## Attempt at cleaning data
library(lubridate)
library(tidyverse)

# Which directories to retrieve data from, and save data to
datdir <- "./p5data"
cldatdir <- "./p5dataclean"
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")

# Vector of directory paths to directories in original data folder
dirvec <- list.dirs(datdir , full.names = 1 , recursive = 0)


# Creating new directories for clean data
for(i in 1:length(dirvec)){
  dir.create(file.path(cldatdir , basename(dirvec[i]) ))
}

# Vector of directory paths to directories in clean data folder
cldirvec <- list.dirs(cldatdir , full.names = 1 , recursive = 0)


# Loop of all directories (stocks)
nDir <- length(dirvec)
for(dirloop in 1:nDir){

  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)
  
  # For loop that cleans each csv of a single stock and saves
  nCsv <- length(csvvec)
  for (csvloop in 1:nCsv){
  
    dat <- read.csv(csvvec[csvloop])
    keepRows <- c("utcsec" , "price")
    dat <- dat[keepRows] ; tail(dat)
    dat$hms <- hms(dat$utcsec) ; tail(dat)
    
    # Time vector in minutes
    tcol    <-    hour(dat$hms)*60   + 
                  minute(dat$hms)    + 
                  second(dat$hms)/60 ; head(tcol)
    
    cldat <- dat[c("utcsec","price")] ; head(cldat)
    floorvec <- floor(tcol)
    secvec   <- second(dat$hms)
    last <- length(dat$utcsec)
    n <- i <- 1
    
    # Cleaning loop
    repeat{
        if(floorvec[n] < floorvec[n+1] ){
          avgPlace <- n
          while(secvec[avgPlace] == secvec[avgPlace-1] &&
                avgPlace > 1) {
            avgPlace <- avgPlace - 1
          }  
          if(avgPlace == n){
            cldat[i,1] <- dat$utcsec[n]
            cldat[i,2] <- dat$price[n]
          }
          else{
            cldat[i,1] <- dat$utcsec[n]
            cldat[i,2] <- sum(dat$price[avgPlace:n])/(n-avgPlace+1)
          }
          i <- i + 1
        }
        n <- n + 1
        if (n == last ){
          cldat[i,1] <- dat$utcsec[n]
          cldat[i,2] <- dat$price[n]
          cldat <- cldat[1:i,]
          break}
    }
    
    
    # Save clean file to new directory
    write.csv(cldat , file = file.path(cldirvec[dirloop] , 
                                       basename(csvvec[csvloop])) )
  }
}