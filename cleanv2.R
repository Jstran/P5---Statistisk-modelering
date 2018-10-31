library(lubridate)
library(stringr)

# Which directories to retrieve data from, and save data to
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
datdir <- "./p5data"
cldatdir <- "./p5dataclean"

# Vector of directory paths to directories in original data folder
dirvec <- list.dirs(datdir , full.names = 1 , recursive = 0)

timevec       <- format(seq(as.POSIXct("2013-01-01 09:30:00", tz="GMT"), 
                 length.out=391, by='1 min'), '%H:%M')
minutes <- hour(hm(timevec))*60 + minute(hm(timevec)) # Vector of time between 09:30 and 16:00 in minutes

for(dirloop in 1:length(dirvec)){
  
  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)
  cldat <- data.frame(numeric(391))
  row.names(cldat) <- timevec

  # Loop that cleans each csv of a single stock and saves all files in a single data frame
    nCsv <- length(csvvec)
    for(csvloop in 1:nCsv){
      #ceil <- 
      dat <- read.csv(csvvec[csvloop])
      dat <- dat[c("utcsec" , "price")]
      
      # Time vector in minutes
      deciminutes    <-    hour(hms(dat$utcsec))*60   + 
                           minute(hms(dat$utcsec))    + 
                           second(hms(dat$utcsec))/60
      
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

      # Main cleaning loop
      repeat{
        while (ceil[n] <= minutes[k] && n < last) {
          n <- n + 1
        }
        avgPlace <- n - 1
        while(avgPlace > 2 && ceil[avgPlace] == ceil[avgPlace - 1] &&
              secvec[avgPlace] == secvec[avgPlace - 1]){
          avgPlace <- avgPlace - 1
        }
        price <- sum(dat$price[avgPlace:(n-1)])/
                 length(dat$price[avgPlace:(n-1)])
        while(ceil[n] > minutes[k]){
          if(avgPlace > 0){pricevec[i] <- price}
          else{pricevec[i] <- dat$price[n-1]}
          i <- i + 1
          k <- i
          if(i == 392){break}
        }
        if(n == last | i == 392){
          avgPlace <- n
          avgPrice <- sum(dat$price[avgPlace:(n)])/(n + 1 - avgPlace)
          while(i < 392){
            pricevec[i] <- avgPrice
            i <- i + 1
          }
          break
        }
      }
      cldat[,csvloop] <- pricevec
      date <- str_extract_all(basename(csvvec[csvloop]), "[0-9]+")
      names(cldat)[csvloop] <- date
  }# End of csvloop
  
  # Saving clean data to .csv
  write.csv( cldat , 
             file = file.path(cldatdir , 
             paste(basename(dirvec[dirloop]),".csv")) )  
}# End of directory loop

  
