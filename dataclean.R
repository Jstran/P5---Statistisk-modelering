## Forsøg på at rense data

#setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")

dat <- read.csv("TROW_20081028.csv")
keepRows <- c("utcsec" , "price" , "volume")
dat <- dat[keepRows] ; tail(dat)
dat$hms <- hms(dat$utcsec) ; tail(dat)

library(lubridate)
library(tidyverse)
library(microbenchmark)

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
        i <- i + 1
      }
      else{
        cldat[i,1] <- dat$utcsec[n]
        cldat[i,2] <- sum(dat$price[avgPlace:n])/(n-avgPlace+1)
        i <- i + 1
      }
    }
    n <- n + 1
    if (n == last ){
      cldat <- cldat[1:i-1,]
      stop("Du er nået enden. Tillykke!") }
}

# Løkke med ?list.dirs og list.files til at ændre filer for alle aktier og alle dage
