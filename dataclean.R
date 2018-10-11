## Forsøg på at rense data
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
library(lubridate)
library(tidyverse)
library(microbenchmark)

# Which directories to retrieve data from, and save data to
datdir <- "./p5data"
cldatdir <- "./p5dataclean"

# Vector of directory paths to directories in original data folder
dirvec <- list.dirs(datdir , full.names = 1 , recursive = 0)


# Creating new directories for clean data
for(i in 1:length(dirvec)){
  dir.create(file.path(cldatdir , basename(dirvec[i]) ))
}

# Vector of directory paths to directories in clean data folder
cldirvec <- list.dirs(cldatdir , full.names = 1 , recursive = 0)


# for loop rundt om dirvec
dirvec[1]
j <- 1

# for loop rundt om csvvec (skal køre indeni 
# hver iteration af løkken ovenfor)
csvvec <- list.files(dirvec[j] , full.names = 1) ; head(csvvec)

nCsv <- length(csvvec)

#####
# For loop that cleans each csv of a single stock and saves
for (k in 1:nCsv){

dat <- read.csv(csvvec[k])
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
      stop("Du er nået enden. Tillykke!") }
}


# Gemmer filen
#write.csv(cldat , file = file.path(cldirvec[j] , basename(csvvec[k])) )
}
