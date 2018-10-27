library(lubridate)

# Which directories to retrieve data from, and save data to
setwd("D:/")
datdir <- "./p5data/data"
cldatdir <- "./p5data/dataclean"

# Vector of directory paths to directories in original data folder
dirvec <- list.dirs(datdir , full.names = 1 , recursive = 0)

timevec       <- format(seq(as.POSIXct("2013-01-01 09:30:00", tz="GMT"), 
                 length.out=391, by='1 min'), '%H:%M')
minutes <- hour(hm(timevec))*60 + minute(hm(timevec)) # Vector of time between 09:30 and 16:00 in minutes

for(dirloop in 1:length(dirvec)){
  
  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)

  # For loop that cleans each csv of a single stock and saves
    for(csvloop in 1:length(csvvec)){
      #ceil <- 
      dat <- read.csv(csvvec[csvloop])
      dat <- dat[c("utcsec" , "price")]
      
      # Time vector in minutes
      deciminutes    <-    hour(hms(dat$utcsec))*60   + 
                           minute(hms(dat$utcsec))    + 
                           second(hms(dat$utcsec))/60
      
      cldat <- dat
      ceil <- ceiling(deciminutes)
      secvec   <- second(hms(dat$utcsec))
      last <- length(dat$utcsec)
      n <- i <- k <- 1
      
      if(ceil[1] > 570){ # Hvis første observation er 09:30:01 eller efter
        if(csvloop == 1){
          while (ceil[n] < ) {
            
          }
        }
        else{
          
        }
      }
      
      
      repeat{
        
      }
    
    
  }
  
  
  
}
  
