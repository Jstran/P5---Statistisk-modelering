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

for(dirloop in 1:1){
  
  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)

  # Loop that cleans each csv of a single stock and saves all files in a single data frame
    for(csvloop in 1:1){
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
      pricevec <- numeric(391)
      n <- i <- k <- 1
      
      if(ceil[1] > 570){ # Hvis første observation er 09:30:01 eller efter
        if(csvloop == 1){ # Hvis det er aktiens første dag
          while (minutes[i] < ceil[1]) {
            pricevec[i] <- dat$price[1]
            i <- i + 1
          }
          k <- i
        }
        else{ # Hvis det ikke er aktiens første dag
          while (minutes[i] < ceil[1]) {
            pricevec[i] <- cldat[391,(csvloop-1)] # TJEK HER OM INDEKS PASSER!
            i <- i + 1
          }
          k <- i
        }
      }
      while (ceil[n] <= 571) {
          n <- n + 1
        }
      # Main cleaning loop
      repeat{
        while (ceil[n] <= minutes[k]) {
          n <- n + 1
        }
        
        
      }
    
    
  }
  
  
  
}
  
