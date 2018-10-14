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





######
# Prøver lige igen
## Attempt at cleaning data
library(lubridate)

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



## Til at vælge dato
# library(stringr)
# date <- str_extract_all(basename(csvvec[1]), "[0-9]+")

# Loop of all directories (stocks)
nDir <- length(dirvec)
for(dirloop in 1:nDir){
  
  # Vector of file names for stock in the current iteration
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)
  
  # For loop that cleans each csv of a single stock and saves
  nCsv <- length(csvvec)
  for (csvloop in 1:nCsv){
    
    #dat <- read.csv(csvvec[csvloop])
    dat <- read.csv("ACAS_19990104.csv")
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
          pricevec[i] <- dat$price[n]
        }
        else{
          pricevec[i] <- sum(dat$price[avgPlace:n])/(n-avgPlace+1)
        }
        i <- i + 1
      }
      n <- n + 1
      if (n == last ){
        pricevec[i] <- dat$price[n]
        break}
    }
    
    # Save clean file to new directory
    write.csv(cldat , file = file.path(cldirvec[dirloop] , 
                                       basename(csvvec[csvloop])) )
  }
}

# n er pos i data der undersøges. i er pos der indsættes i clean data og k er pos i minutetimevec
n <- i <- k <- 1
last <- length(dat$utcsec)
pricevec <- numeric(391)
repeat{
  while(floorvec[n] < 570 ){ # Klokken er mindre end 09:30
    n <- n + 1
  }
  while(floorvec[n] == minutetimevec[k]){ # Problemer hvis der ikke er data fra 9:30
  n <- n + 1
  }
  avgPlace <- n - 1
  while(secvec[avgPlace] == secvec[avgPlace-1] &&
        avgPlace > 1) {
    avgPlace <- avgPlace - 1
  }
  avgPrice <- sum(dat$price[avgPlace:n-1])/(n-avgPlace+1) # Skal undersøges om den udregner rigtigt nu
  if(floorvec[n] == minutetimevec[k+1]){
    if(avgPlace == n - 1){
      pricevec[i] <- dat$price[n]
    }
    else{
      pricevec[i] <- avgPrice
    }
    i <- i + 1
    k <- k + 1
  }
  else{
    while(floorvec[n] > minutetimevec[k]){
      if(avgPlace == n - 1){
        pricevec[i] <- dat$price[n]
      }
      else{
        pricevec[i] <- avgPrice
      }
      i <- i + 1
      k <- k + 1
    }
    
  }
  if(n == last | floorvec[n] > 959){break}
}

pricevec

k
n

x <- data.frame(y = c(5,6), z = c(8,9)); x
prices <- c(10,12)
datonavn <- "19990204"
x[,3] <- prices ; x
names(x)[3] <- paste(datonavn) ; x

# Tidsvektor
timevec       <- format(seq(as.POSIXct("2013-01-01 09:30:00", tz="GMT"), 
                 length.out=391, by='1 min'), '%H:%M')
minutetimevec <- hour(hm(timevec))*60 + minute(hm(timevec))
minutetimevec
