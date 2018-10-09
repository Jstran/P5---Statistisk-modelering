## Forsøg på at rense data

setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")

dat <- read.csv("p5data/TROW/TROW_20081028.csv")
keepRows <- c("utcsec" , "price" , "volume")
dat <- dat[keepRows] ; tail(dat)
dat$hms <- hms(dat$utcsec) ; tail(dat)

library(lubridate)
library(tidyverse)


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

repeat{
  if(minute(dat$hms[n]) < minute(dat$hms[n+1]) ){
    avgPlace <- n
    avgDenom <- 1
    avgNumerat <- dat$price[n]
    
    while(second(dat$hms[avgPlace]) == second(dat$hms[avgPlace-1]) &&
          avgPlace > 1) {
      # Denne while-løkke vil nok ikke fungere hvis der kun er én
      # observation i det første minut.
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

