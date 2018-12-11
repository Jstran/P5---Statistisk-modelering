
library(lubridate)
library(stringr)
# Which directories to retrieve data from, and save data to
datdir <- "./p5data"
cldatdir <- "./p5dataclean"
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")

# Tidsvektor
timevec       <- format(seq(as.POSIXct("2013-01-01 09:30:00", tz="GMT"), 
                            length.out=391, by='1 min'), '%H:%M')
minutetimevec <- hour(hm(timevec))*60 + minute(hm(timevec))
minutetimevec <- c(minutetimevec , 961)


# Vector of directory paths to directories in original data folder
dirvec <- list.dirs(datdir , full.names = 1 , recursive = 0)


# Creating new directories for clean data
for(i in 1:length(dirvec)){
  dir.create(file.path(cldatdir , basename(dirvec[i]) ))
}

# Vector of directory paths to directories in clean data folder
cldirvec <- list.dirs(cldatdir , full.names = 1 , recursive = 0)



## Til at vælge dato
# library(stringr)
# date <- str_extract_all(basename(csvvec[1]), "[0-9]+")

# Loop of all directories (stocks)
nDir <- length(dirvec)
for(dirloop in 1:nDir){
  
  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)
  
  cldat <- data.frame(timevec)
  
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
    
    floorvec <- floor(tcol)
    secvec   <- second(dat$hms)
    
    # 'n' er pos i data der undersøges. 'i' er pos der 
    # indsættes i clean data og 'k' er pos i minutetimevec
    n <- i <- k <- 1
    last <- length(dat$utcsec)
    pricevec <- numeric(391)

    if(csvloop == 1){ # Hvis data før 9:30, indsættes sidste observation før 9:30
      if(floorvec[1] < 570){
        while(floorvec[n] < 570){n <- n + 1}
        pricevec[1] <- dat$price[n-1]
        i <- 2
      }
      if(floorvec[1] > 569){ # Hvis intet data før 9:30, indsættes...
       while(minutetimevec[k] < floorvec[1] + 1){
         pricevec[i] <- dat$price[1]
         k <- k + 1
         i <- i + 1
       }
      }
    }
    else{
      if(floorvec[1] < 570){
        while(floorvec[n] < 570){n <- n + 1}
        pricevec[1] <- dat$price[n-1]
        i <- 2
      }
      if(floorvec[1] > 569){
        while(minutetimevec[k] < floorvec[1] + 1){
          pricevec[i] <- cldat[391,csvloop]
          k <- k + 1
          i <- i + 1
        }
      } 
    }
  
    repeat{
    #  while(floorvec[n] < 570 ){ # Klokken er mindre end 09:30
    #    n <- n + 1
    #  }
      
      while(floorvec[n] < minutetimevec[k] + 1 & n < last){ # Problemer hvis der ikke er data fra 9:30
      n <- n + 1
      }
      avgPlace <- n - 1
      while(avgPlace > 2 && secvec[avgPlace] == secvec[avgPlace-1]
            && floorvec[avgPlace] == floorvec[avgPlace - 1]){
        avgPlace <- avgPlace - 1
      }
      avgPrice <- sum(dat$price[avgPlace:(n-1)])/(n-avgPlace) # Skal undersøges om den udregner rigtigt nu
      
      if(floorvec[n] == minutetimevec[k+1]){ # 391 er længden af minutetimevec
          pricevec[i] <- avgPrice
        i <- i + 1
        k <- k + 1
      }
      else{
        while(floorvec[n] > minutetimevec[k]){
          if(avgPlace == n - 1 & avgPlace > 0){
            pricevec[i] <- dat$price[avgPlace]
          }
          if(avgPlace == 0){
            pricevec[i] <- dat$price[1]
          }
          else{
            pricevec[i] <- avgPrice
          }
          i <- i + 1
          k <- k + 1
          if(i == 392){break}
        }
    
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
    
    cldat[,csvloop + 1] <- pricevec
    date <- str_extract_all(basename(csvvec[csvloop]), "[0-9]+")
    names(cldat)[csvloop + 1] <- date

  } # Slut paa csv loekke.
write.csv( cldat , file = file.path(cldirvec[dirloop] , paste(basename(cldirvec[dirloop]),".csv")) )
} # Slut paa dir loekke.
