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

pricevec <- c(0)
n <- 1
i <- 1
#cldat <- data.frame(time = dat$utcsec[1] , price = dat$price[1])
cldat <- dat[c("utcsec","price")]
head(cldat)

repeat{
  if ( ceiling(tcol[n]) < ceiling(tcol[n+1]) ){
    avgPlace <- n
    avgDenom <- 1
    avgNumerat <- dat$price[n]
    
    while(tcol[n] == tcol[n-1] && avgPlace > 1){
      avgNumerat <- avgNumerat + dat$price[avgPlace]
      avgDenom <- avgDenom + 1
      avgPlace <- avgPlace - 1
    }
    if(avgDenom == 1){
      cldat[i,1] <- dat$utcsec[n]
      cldat[i,2] <- dat$price[n]
      i <- i + 1
    }
    else{
      cldat[i,1] <- dat$utcsec[n]
      cldat[i,2] <- avgNumerat/avgDenom
      i <- i + 1
    }
  }
  n <- n + 1
  if (n == length(dat$utcsec) ){
    cldat <- cldat[1:i-1,]
    stop("Du er nået enden. Tillykke!")}
}

head(cldat)
tail(cldat)
cldat

n <- i <- 1
microbenchmark(
repeat{
  if(minute(dat$hms[n]) < minute(dat$hms[n+1]) ){
    avgPlace <- n
    
    while(second(dat$hms[avgPlace]) == second(dat$hms[avgPlace-1]) &&
          avgPlace > 1) {
      avgPlace <- avgPlace - 1
    }  
    if(avgPlace < n){
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
  if (n == length(dat$utcsec) ){
    cldat <- cldat[1:i-1,]
    stop("Du er nået enden. Tillykke!")}
}
)
head(cldat)
tail(cldat)
cldat



cldat_v2 <- dat[c("utcsec","price")]
n <- i <- 1
system.time(
  repeat{
    if(floor(tcol[n]) < floor(tcol[n+1]) ){
      avgPlace <- n
      while(second(dat$hms[avgPlace]) == second(dat$hms[avgPlace-1]) &&
            avgPlace > 1) {
        avgPlace <- avgPlace - 1
      }  
      if(avgPlace == n){
        cldat_v2[i,1] <- dat$utcsec[n]
        cldat_v2[i,2] <- dat$price[n]
        i <- i + 1
      }
      else{
        cldat_v2[i,1] <- dat$utcsec[n]
        cldat_v2[i,2] <- sum(dat$price[avgPlace:n])/(n-avgPlace+1)
        i <- i + 1
      }
    }
    n <- n + 1
    if (n == length(dat$utcsec) ){
      cldat_v2 <- cldat_v2[1:i-1,]
      stop("Du er nået enden. Tillykke!")}
  }
)  
head(cldat_v2)
nrow(cldat_v2)





cldat_v3 <- dat[c("utcsec","price")] ; head(cldat_v3)
floorvec <- floor(tcol)
secvec   <- second(dat$hms)
last <- length(dat$utcsec)
n <- i <- 1
system.time(
  repeat{
    if(floorvec[n] < floorvec[n+1] ){
      avgPlace <- n
      while(secvec[avgPlace] == secvec[avgPlace-1] &&
            avgPlace > 1) {
        avgPlace <- avgPlace - 1
      }  
      if(avgPlace == n){
        cldat_v3[i,1] <- dat$utcsec[n]
        cldat_v3[i,2] <- dat$price[n]
        i <- i + 1
      }
      else{
        cldat_v3[i,1] <- dat$utcsec[n]
        cldat_v3[i,2] <- sum(dat$price[avgPlace:n])/(n-avgPlace+1)
        i <- i + 1
      }
    }
    n <- n + 1
    if (n == last ){
      cldat_v3 <- cldat_v3[1:i-1,]
      stop("Du er nået enden. Tillykke!")}
  }
)  
head(cldat_v3)
tail(cldat_v3)
nrow(cldat_v3)
