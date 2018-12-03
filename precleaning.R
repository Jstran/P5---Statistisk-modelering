### Pakker
library(zoo)
library(microbenchmark)
library(lubridate)
library(stringr)

### Mapper der arbejdes med
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
dirvec <- list.dirs("./rawdata" , full.names = 1 , recursive = 0)
dat <- read.csv("./p5data/ACAS/ACAS_20050104.csv")

### Løkker
system.time(
for(dirloop in 1:1){ # Start på dirloop 
  csvvec <- list.files(dirvec[dirloop] , full.names = 1)
  pricevec <- c()
  
  for(csvloop in 1:length(csvvec)){ # Start på csvloop
    # Indlæser en aktiedag
    dat <- read.csv(csvvec[csvloop]) #; print(tail(dat))
    
    ### Data før 09:30 og efter 16:00 ---------------------------------
    # Omregner minutter til decimaltal og afrunder
    deciminutes <- hour(hms(dat$utcsec))*60   + 
                   minute(hms(dat$utcsec))    +
                   second(hms(dat$utcsec))/60
    floormin    <- floor(deciminutes)
    ceilmin     <- ceiling(deciminutes)
    
    # Fjerner data før 09:30 og efter 16:00
    keep <- (floormin >= 570 & ceilmin <= 960)
    dat <- dat[keep,] 
    
    ### Priser der er 0 -----------------------------------------------
    keep <- (dat$price > .Machine$double.eps)
    dat <- dat[keep,]
    
    ### Corr der er != 0 ----------------------------------------------
    keep <- (abs(dat$corr) < .Machine$double.eps )
    dat <- dat[keep,] ;
    
    ### Cond der er et bogstav != E eller F ---------------------------
    
    # Fjerner mellemrum fra dat$cond
    datcond <- gsub(" ", "", dat$cond, fixed = TRUE)
    
    # Beholder "E" , "F" og "". ## DET HER ER IKKE RIGTIGT HUSK TAL
    keep <- datcond %in% c("E","F","@","")
    dat <- dat[keep,] #; print(tail(dat))
    
    ### Priser og intervaller -----------------------------------------
    # Tilføjer priser til pricevec og gemmer intervaller for aktie-
    # dagen
    pricevec <- c(pricevec , dat$price)
    if(csvloop == 1){
      startint <- 1
      endint <- length(dat$price)
    }
    else{
      startint <- c(startint , endint[length(endint)] + 1)
      endint   <- c(endint , endint[csvloop - 1] + 1 + 
                             length(dat$price))
    }
    print(csvloop)
    } # Slut på csvloop
  
  ### MAD -------------------------------------------------------------
  print(length(pricevec))
  madresults <- rollapply(pricevec , 
                          width = 51 , 
                          FUN = mad , 
                          constant = 1)
  
  #madresults <- c()
  #for(i in 26:(length(pricevec) - 26 ) ){
  #    madresults <- c(madresults , 
  #                    mad(pricevec[(i - 25) : (i + 25)] , constant = 1))
  #    print(i)
  #}
  #madresults <- lapply(madframe , FUN = mad , constant = 1)
  
} # Slut på dirloop
)

startint
endint
pricevec

### TEST --------------------------------------------------------------

testframe <- data.frame(x = c(1,2,3) , y = c(4,6,8) , z = c(9,12,15))
testframe

apply(testframe , MARGIN = 2, FUN = mad)
lapply(testframe , FUN = mad , constant = 1)
mad(c(9,12,15) , constant = 1)


microbenchmark(
  apply(testframe , MARGIN = 2, FUN = mad , constant = 1) ,
  lapply(testframe , FUN = mad , constant = 1) ,
  datcond <- gsub(" ", "", dat$cond, fixed = TRUE),
  unit = "ms"
)

#rapply(as.list(pricevec[1:50000]) , width = 51 , FUN = mad , constant = 1)
rollapply(c(1,8,6,3,5,9,8,5) , width = 3 , FUN = mad , constant = 1)
mad(c(1,8,6) , constant = 1)
mad(c(8,6,3) , constant = 1)
mad(c(6,3,5) , constant = 1)
mad(c(3,5,9) , constant = 1)
mad(c(5,9,8) , constant = 1)
mad(c(9,8,5) , constant = 1)
