## Forsøg på at rense data

setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")

testdata <- read.csv("p5data/TROW/TROW_20081028.csv"); tail(testdata)
keepRows <- c("utcsec" , "price" , "volume")
testdata <- testdata[keepRows] ; tail(testdata)
# head(testdata)
# summary(testdata)
# str(testdata)
library(lubridate)
library(tidyverse)

hmscol  <- hms(testdata$utcsec) ; head(hmscol)
tcol    <-    hour(hmscol)*60 + 
              minute(hmscol)  + 
              second(hmscol)/60 ; head(tcol)

nytid <- c(0)
n <- 1
i <- 1
repeat{
  if ( ceiling(tcol[n]) < ceiling(tcol[n+1])) {
   nytid[i] <- testdata$price[n]
   i <- i +1
  }
  n <- n + 1
  if (n == length(tcol)){stop("Den er blevet stoppet")}
}
nytid
k <- c(1,3) ; k
k[3] <- 5 ; k
