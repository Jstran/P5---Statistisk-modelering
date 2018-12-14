### (1)  Intro ------------------------------------------------------

# Pakker
library(openxlsx)
library(lubridate)
library(lmtest)

# Mapper der bruges
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
cldatdir <- "./p5dataclean"

# Navne og placeringer paa aktier
csvvec <- list.files(cldatdir , full.names = 1 , recursive = 0)
snames <- substr(basename(csvvec),1,nchar(basename(csvvec))-5)

mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

### (2)  Sampling frekvens -------------------------------------------

priceseq <- seq(1,391,5)

### (3)  stockreader funktion ---------------------------------------

# Funktion der indlaeser aktier nr 'stockind' i csvvec, og indsaetter
# det i data.frame (vektor hvis onlyreturn = TRUE), hvori overnight
# returns er fjernet. Det er for 5 min afkast
stockreader <- function(stockind , onlyreturn = FALSE){
  
  pricevec <- c()
  dat <- read.csv(csvvec[stockind])
  
  if(onlyreturn == FALSE){
    datevec  <- c()
    
    for(i in 1:(length(dat) - 1) ){
      pricevec <- c(pricevec , dat[priceseq,(i+1)])
      datevec  <- c(datevec , rep(names(dat)[(i+1)], 79) )
    }
    datevec <- ymd(substring(datevec,2))
    stock   <- data.frame(date = datevec[2:length(datevec)] , 
                          price = pricevec[2:length(pricevec)] ,
                          return = diff(log(pricevec))  )
    keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
    stock <- stock[keep,]
  }
  if(onlyreturn == TRUE){
    
    for(i in 1:(length(dat) - 1) ){
      pricevec <- c(pricevec , dat[priceseq,(i+1)])
    }
    stock   <- diff(log(pricevec))
    keep <- (1:length(stock))[!1:length(stock) %in% ((1:2766)*79)]
    stock <- stock[keep]
  }
  return(stock)
}


### (4)  SPY indlaesning ----------------------------------------------

SPYind <- 28 ; snames[28]
SPY <- stockreader(SPYind)

### (5)  HML og SMB indlaesning ---------------------------------------

# Mappe
factdir <- "./factors"
factcsv <- list.files(factdir , full.names = 1 , recursive = 0)
factcsv

# HML
dat <- read.xlsx(factcsv[1])
HML <- data.frame(date = ymd(dat$X1) , return = log(1 + dat$X3))
HML <- HML[2:length(HML$return),]

# Til at fjerne overnight return
keep <- (1:length(HML$return))[!1:length(HML$return) %in% ((1:2766)*79)] 
HML <- HML[keep,]

# SMB
dat <- read.xlsx(factcsv[2])
SMB <- data.frame(date = ymd(dat$X1) , return = log(1 + dat$X3))
SMB <- SMB[2:length(SMB$return),]
SMB <- SMB[keep,]

### (6)  Naiv portefoelje (5 min) -------------------------------------

# Laver naiv portefoelje data.frame
allstock <- data.frame(numeric(215748))
j <- 1
datevec <- c()

for(k in (1:length(snames))[-28]){ # Aktier uden SPY
  
  if(k == 1){
    dat <- read.csv(csvvec[k])
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)], 79))  
      
    }
    keep <- (1:length(datevec))[!1:length(datevec) %in% ((1:2766)*79)]
    datevec <- (datevec[2:length(datevec)])[keep]
    datevec <- ymd(substring(datevec,2))
  }
  
  allstock[,j] <- stockreader(k , onlyreturn = TRUE)
  names(allstock)[j] <- snames[k]
  j <- j + 1
  print(k)
}
naiveport <- data.frame(date   = datevec ,
                        return = 1/35 * rowSums( allstock ) )

### (7)  R^2 tabel --------------------------------------------------

radjframe <- data.frame(X2004 = rep(0,36) , 
                        X2008 = rep(0,36) , 
                        Gns   = rep(0,36))
row.names(radjframe) <- c(snames[-28] , "Port")

j <- 1
for(stockind in (1:length(snames))[-28]){
  
  radj <- c()
  stock <- stockreader(stockind)

  for(y in 1999:2009 ){
    for(m in 1:12){
      int <- year(stock$date) == y & month(stock$date) == m 
      k <- length(stock$return[int])
      h <- 1/k * rep(1,k)
    
      famamod <- lm( stock$return[int] ~ h               +
                                         SPY$return[int] + 
                                         HML$return[int] + 
                                         SMB$return[int] -
                                         1)
      
      radj <- c(radj , summary(famamod)$adj.r.squared)
      if(m == 6){
        if(y == 2004){
          radjframe$X2004[j] <- summary(famamod)$adj.r.squared
        }
        if(y == 2008){
          radjframe$X2008[j] <- summary(famamod)$adj.r.squared
        }
      }
    }
  }
radjframe$Gns[j] <- mean(radj)
print(j)
j <- j + 1
}

radj <- c()
for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    
    
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1,k)
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)

    radj <- c(radj , summary(famamod)$adj.r.squared )
    if(m == 6){
      if(y == 2004){
        radjframe$X2004[j] <- summary(famamod)$adj.r.squared
      }
      if(y == 2008){
        radjframe$X2008[j] <- summary(famamod)$adj.r.squared
      }
    }
  }
radjframe$Gns[j] <- mean(radj)
}
radjframe

### (8)  Cor mellem faktorer og residualer --------------------------

corframe <- data.frame(SPY = numeric(9) , 
                       HML = numeric(9) ,
                       SMB = numeric(9))
row.names(corframe)[7:9] <- c("Portefølje 2004" , "Portefølje 2008" ,
                              "Portefølje Gns")

# Aktie 1
stockind   <- 7 ; print(csvvec[stockind])
stock      <- stockreader(stockind)

row.names(corframe)[1:3] <- c(paste(snames[stockind] ," 2004" , sep = "") ,
                              paste(snames[stockind] ," 2008" , sep = "") , 
                              paste(snames[stockind] ," Gns"  , sep = ""))

j <- 1
cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m 
    k <- length(stock$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( stock$return[int] ~ h               +
                                       SPY$return[int] + 
                                       HML$return[int] + 
                                       SMB$return[int] -
                                       1)
    
    cormeanSPY <- c(cormeanSPY , cor(famamod$residuals , SPY$return[int]))
    cormeanHML <- c(cormeanHML , cor(famamod$residuals , HML$return[int]))
    cormeanSMB <- c(cormeanSMB , cor(famamod$residuals , SMB$return[int]))
    if(m == 6){
      if(y == 2004){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
      if(y == 2008){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
    }
  }
}
corframe[j,] <- c(mean(cormeanSPY) , 
                  mean(cormeanHML) , 
                  mean(cormeanSMB) )
j <- j + 1



# Aktie 2
stockind   <- 24 ; print(csvvec[stockind])
stock      <- stockreader(stockind)
row.names(corframe)[4:6] <- c(paste(snames[stockind] ," 2004" , sep = "") ,
                              paste(snames[stockind] ," 2008" , sep = "") , 
                              paste(snames[stockind] ," Gns" , sep =""))

cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m 
    k <- length(stock$return[int])
    h <- 1/k * rep(1,k)
    
    famamod <- lm( stock$return[int] ~ h               +
                                       SPY$return[int] + 
                                       HML$return[int] + 
                                       SMB$return[int] -
                                       1)
    
    cormeanSPY <- c(cormeanSPY , cor(famamod$residuals , SPY$return[int]))
    cormeanHML <- c(cormeanHML , cor(famamod$residuals , HML$return[int]))
    cormeanSMB <- c(cormeanSMB , cor(famamod$residuals , SMB$return[int]))
    if(m == 6){
      if(y == 2004){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
      if(y == 2008){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
    }
  }
}
corframe[j,] <- c(mean(cormeanSPY) , 
                  mean(cormeanHML) , 
                  mean(cormeanSMB) )
j <- j + 1

# Portefoelje
cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m 
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)
    
    cormeanSPY <- c(cormeanSPY , cor(famamod$residuals , SPY$return[int]))
    cormeanHML <- c(cormeanHML , cor(famamod$residuals , HML$return[int]))
    cormeanSMB <- c(cormeanSMB , cor(famamod$residuals , SMB$return[int]))
    if(m == 6){
      if(y == 2004){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
      if(y == 2008){
        corframe[j,] <- c( cor(famamod$residuals , SPY$return[int]) ,
                           cor(famamod$residuals , HML$return[int]) ,
                           cor(famamod$residuals , SMB$return[int]))
        j <- j + 1
      }
    }
  }
}
corframe[j,] <- c(mean(cormeanSPY) , 
                  mean(cormeanHML) , 
                  mean(cormeanSMB) )
print(corframe)

### (9)  Breusch-Pagan tests ----------------------------------------

bppframe <- data.frame(Pleq = numeric(3) , 
                       Pg   = numeric(3))
row.names(bppframe)[3] <- "Portefølje"

# Aktie 1
stockind   <- 7 ; print(csvvec[stockind])
stock <- stockreader(stockind)

j <- 1
bpps <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m 
    k <- length(stock$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( stock$return[int] ~ h               +
                                       SPY$return[int] + 
                                       HML$return[int] + 
                                       SMB$return[int] -
                                       1)
    bpps <- c(bpps , as.numeric( bptest(famamod)$p.value) )
  }
}
bppframe[j,] <- c(length(bpps[bpps <= 0.05]) ,
                  length(bpps[bpps >  0.05]) )
row.names(bppframe)[j] <- snames[stockind]
j <- j + 1

#plot(bpps , col = "red")
#abline( h = 0.05 )
#points((1:132)[bpps > 0.05] , bpps[bpps > 0.05] , col = "blue")


# Aktie 2
stockind   <- 24 ; print(csvvec[stockind])
stock <- stockreader(stockind)

bpps <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m
    k <- length(stock$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( stock$return[int] ~ h               +
                                       SPY$return[int] + 
                                       HML$return[int] + 
                                       SMB$return[int] -
                                       1)
    bpps <- c(bpps , as.numeric( bptest(famamod)$p.value) )
  }
}
bppframe[j,] <- c(length(bpps[bpps <= 0.05]) ,
                  length(bpps[bpps >  0.05]) )
row.names(bppframe)[j] <- snames[stockind]
j <- j + 1

setEPS()
postscript( paste("bpp",snames[stockind],".eps",sep = "") ,
            height = 6)
plot(bpps , 
     col = "red" , 
     xlab = "Måneder efter januar 1999" ,
     ylab = "P-værdi")
abline( h = 0.05 )
points((1:132)[bpps > 0.05] , bpps[bpps > 0.05] , col = "blue")
dev.off()

# Portefoelje
bpps <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m 
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)
    bpps <- c(bpps , as.numeric( bptest(famamod)$p.value) )
  }
}
bppframe[j,] <- c(length(bpps[bpps <= 0.05]) ,
                  length(bpps[bpps >  0.05]) )
print(bppframe)

#plot(bpps , col = "red")
#abline( h = 0.05 )
#points((1:132)[bpps > 0.05] , bpps[bpps > 0.05] , col = "blue")

### (10) R^2 Fama v CAPM5 v CAPM1 -----------------------------------

rsqframe <- data.frame(X2004 = numeric(3) ,
                       X2008 = numeric(3) ,
                       Gns   = numeric(3))
row.names(rsqframe) <- c("Fama (5 min)" , 
                         "CAPM (5 min)" , 
                         "CAPM (1 min)")

# Fama og CAPM for 5 min
rsqfama  <- c()
rsqcapm5 <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1,k) 
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)
    capmmod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] -
                                           1)
    
    rsqfama  <- c(rsqfama  , summary(famamod)$adj.r.squared)
    rsqcapm5 <- c(rsqcapm5 , summary(capmmod)$adj.r.squared )
    if(m == 6){
      if(y == 2004){
        rsqframe$X2004[1] <- summary(famamod)$adj.r.squared
        rsqframe$X2004[2] <- summary(capmmod)$adj.r.squared
      }
      if(y == 2008){
        rsqframe$X2008[1] <- summary(famamod)$adj.r.squared
        rsqframe$X2008[2] <- summary(capmmod)$adj.r.squared
      }
    }
  }
}
rsqframe$Gns[1:2] <- c(mean(rsqfama) , mean(rsqcapm5))

# Portefoelje for 1 min
allstock <- data.frame(numeric(1081505))
j <- 1
datevec <- c()

for(k in (1:36)[!1:36 %in% 28]){ # Aktier uden SPY
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , dat[, (i+1)] )
  }
  if(k == 1){ # Korteste aktie
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)],391))  
      
    }
    datevec <- ymd(substring(datevec,2))
  }
  print(length(pricevec))
  allstock[,j] <- diff(log(pricevec))
  names(allstock)[j] <- snames[k]
  j <- j + 1
  print(k)
}
naiveport1 <- data.frame(date = datevec[2:1081506] ,
                        return = (1/35) * rowSums(allstock) )
keep <- (1:length(naiveport1[,1]))[!1:length(naiveport1[,1]) %in% ((1:2766)*391)]
naiveport1 <- naiveport1[keep,]

# SPY for 1 min
SPYprice <- c()
SPYdate  <- c()

for(i in 1:(length(dat) - 1) ){
  SPYprice <- c(SPYprice , as.numeric(dat[,(i+1)]) ) 
  SPYdate  <- c(SPYdate , rep(names(dat)[(i+1)],391) )
}
SPYdate <- ymd(substring(SPYdate , 2))
SPY1    <- data.frame(date   = SPYdate[1:(length(SPYdate)-1)] , 
                      price  = SPYprice[1:(length(SPYprice)-1)] ,
                      return = diff(log(SPYprice)) )
keep <- (1:length(SPY1[,1]))[!1:length(SPY1[,1]) %in% ((1:2766)*391)]
SPY1 <- SPY1[keep,]

# CAPM for 1 min
rsqcapm1 <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(naiveport1$date) == y & month(naiveport1$date) == m
    k <- length(naiveport1$return[int])
    h <- 1/k * rep(1,k)

    capmmod <- lm( naiveport1$return[int] ~ h                +
                                            SPY1$return[int] -
                                            1)
    
    rsqcapm1 <- c(rsqcapm1 , summary(capmmod)$adj.r.squared )
    if(m == 6){
      if(y == 2004){
        rsqframe$X2004[3] <- summary(capmmod)$adj.r.squared
      }
      if(y == 2008){
        rsqframe$X2008[3] <- summary(capmmod)$adj.r.squared
      }
    }
  }
}
rsqframe$Gns[3] <- mean(rsqcapm1)

print(rsqframe)

plot(rsqfama , type = "l" , col = "black" , ylim = c(0 , 0.9))
lines(rsqcapm5 , col = "blue")
lines(rsqcapm1 , col = "red")

plot(rsqfama - rsqcapm5 , type = "l" , ylim = c(-0.1,0.3))
abline( h = 0)

### (11) P-frame fra t-test ----------------------------------------

pframe <- data.frame(pleq = numeric(3) ,
                     pg   = numeric(3))

j <- 1
for(stockind in c(7,24)){
  
  stock <- stockreader(stockind) ; print(snames[stockind])
  row.names(pframe)[j] <- snames[stockind]
  pvals <- c()
  
  for(y in 1999:2009){
    for(m in 1:12){
      int <- year(stock$date) == y & month(stock$date) == m
      k <- length(stock$return[int])
      h <- 1/k * rep(1 , k)
      
      famamod   <- lm( stock$return[int]   ~ h               +
                                             SPY$return[int] + 
                                             HML$return[int] + 
                                             SMB$return[int] -
                                             1)

      pvals <- c(pvals , summary(famamod)$coefficients[1,4])
    }
  }
  pmns <- length(pvals[pvals <= 0.05])
  pframe[j,] <- c(pmns , 132 - pmns)
  j <- j + 1
}
pvals <- c()

for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    famamod   <- lm( naiveport$return[int]   ~ h               +
                                               SPY$return[int] + 
                                               HML$return[int] + 
                                               SMB$return[int] -
                                               1)
    
    pvals <- c(pvals , summary(famamod)$coefficients[1,4])
  }
}
pmns <- length(pvals[pvals <= 0.05])
pframe[j,] <- c(pmns , 132 - pmns)
row.names(pframe)[j] <- "Portefølje"
print(pframe)


### (12) D-frame fra dwtest -----------------------------------------

dframe <- data.frame(dinint      = numeric(3) ,
                     dnotinint   = numeric(3))
dupper <- 2.2
dlower <- 1.8

j <- 1
for(stockind in c(7 , 24)){
  
  stock <- stockreader(stockind) ; print(snames[stockind])
  row.names(dframe)[j] <- snames[stockind]
  pvals <- c()
  dvals <- c()
  
  for(y in 1999:2009){
    for(m in 1:12){
      int <- year(stock$date) == y & month(stock$date) == m
      k <- length(stock$return[int])
      h <- 1/k * rep(1 , k)
      
      famamod   <- lm( stock$return[int]   ~ h               +
                                             SPY$return[int] + 
                                             HML$return[int] + 
                                             SMB$return[int] -
                                             1)
      
      pvals <- c(pvals , dwtest(famamod)$p.value )
      dvals <- c(dvals , as.numeric(dwtest(famamod)$statistic )) 
    }
  }
  dint <- (dvals <= dupper & dvals >= dlower)
  dmns <- length(dvals[dint])
  dframe[j,] <- c(dmns , 132 - dmns)
  j <- j + 1
}

pvals <- c()
dvals <- c()
for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    famamod   <- lm( naiveport$return[int]   ~ h               +
                                               SPY$return[int] + 
                                               HML$return[int] + 
                                               SMB$return[int] -
                                               1)
                            
    pvals <- c(pvals , dwtest(famamod)$p.value)
    dvals <- c(dvals , as.numeric(dwtest(famamod)$statistic )) 
  }
}
dint <- (dvals <= dupper & dvals >= dlower)
dmns <- length(dvals[dint])
dframe[j,] <- c(dmns , 132 - dmns)
row.names(dframe)[j] <- "Portefølje"
print(dframe)

plot(pvals)
points((1:132)[pvals <= 0.05] , pvals[pvals <= 0.05] ,col = "red")
abline( h = 0.05 )

plot(dvals , ylim = c(1.2 , 2.8) , col = "red")
points((1:132)[dint] , dvals[dint] ,col = "black")
abline( h = dlower )
abline( h = dupper )

### (13) R^2 for enkelte faktorer -----------------------------------

rframe <- data.frame(X2004 = numeric(3) ,
                     X2008 = numeric(3) ,
                     Gns   = numeric(3))
row.names(rframe) <- c("SPY" , "HML" , "SMB") ; rframe

rSPY <- rHML <- rSMB <- c()
for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    SPYmod <- lm( SPY$return[int] ~ h + HML$return[int] +
                                    SMB$return[int] - 1) 
    
    HMLmod <- lm( HML$return[int] ~ h + SPY$return[int] +
                                    SMB$return[int] - 1)
    
    SMBmod <- lm( SMB$return[int] ~ h + SPY$return[int] +
                                    HML$return[int] - 1)
    
    rSPY <- c(rSPY , summary(SPYmod)$adj.r.squared)
    rHML <- c(rHML , summary(HMLmod)$adj.r.squared)
    rSMB <- c(rSMB , summary(SMBmod)$adj.r.squared) 
  }
}
rframe[1,] <- c(rSPY[66] , rSPY[114] , mean(rSPY))
rframe[2,] <- c(rHML[66] , rHML[114] , mean(rHML))
rframe[3,] <- c(rSMB[66] , rSMB[114] , mean(rSMB))
print(rframe)
### (14) QQ residual plots ------------------------------------------

stock <- stockreader(1) ; snames[1]
# Aktie model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(stock$date) == y & month(stock$date) == m 
  k <- length(stock$return[int])
  h <- 1/k * rep(1,k) 
  
  famamod <- lm( stock$return[int] ~ h               +
                                     SPY$return[int] + 
                                     HML$return[int] + 
                                     SMB$return[int] -
                                     1)
  
  setEPS()
  postscript( paste("qqresid",snames[stockind],y,0,m,".eps",sep = "") ,
              height = 6)
  plot(famamod , 
       which = 2 ,             # QQplot
       sub.caption = "" ,      # Ingen sub.caption
       xlab = "Teoretiske kvantiler" ,
       main = paste(snames[stockind]," - ",mns[m]," ",y,sep="" ) )
  dev.off()
}


# Portefoelje model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(naiveport$date) == y & month(naiveport$date) == m
  k <- length(naiveport$return[int])
  h <- 1/k * rep(1,k)
    
  famamod <- lm( naiveport$return[int] ~ h               +
                                         SPY$return[int] + 
                                         HML$return[int] + 
                                         SMB$return[int] -
                                         1)
  setEPS()
  postscript( paste("qqresidport",y,0,m,".eps", sep = "") , height = 6)
  plot(famamod , 
       which = 2 ,             # QQplot
       sub.caption = "" ,      # Ingen sub.caption
       xlab = "Teoretiske kvantiler" ,
       main = paste("Portefoelje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}

### (15) Skedastitetsplots ------------------------------------------

stockind <- 7
stock <- stockreader(stockind) ; snames[stockind]
# Aktie model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(stock$date) == y & month(stock$date) == m 
  k <- length(stock$return[int])
  h <- 1/k * rep(1,k) 
  
  famamod <- lm( stock$return[int] ~ h               +
                                     SPY$return[int] + 
                                     HML$return[int] + 
                                     SMB$return[int] -
                                     1)
  
  setEPS()
  postscript( paste("skedaresid",snames[stockind],y,0,m,".eps",sep = "") ,
              height = 6)
  plot(famamod , 
       which = 1 ,             # Skedastisitetsplot
       sub.caption = "" ,      # Ingen sub.caption
       #xlab = "Fittede værdier" ,
       main = paste(snames[stockind]," - ",mns[m]," ",y,sep="" ) )
  dev.off()
}


# Portefoelje model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(naiveport$date) == y & month(naiveport$date) == m
  
  
  k <- length(naiveport$return[int])
  h <- 1/k * rep(1,k)
  
  famamod <- lm( naiveport$return[int] ~ h               +
                                         SPY$return[int] + 
                                         HML$return[int] + 
                                         SMB$return[int] -
                                         1)
  setEPS()
  postscript( paste("skedaresidport",y,0,m,".eps", sep = "") , height = 6)
  plot(famamod , 
       which = 1 ,             # Skedastisitetsplot
       sub.caption = "" ,      # Ingen sub.caption
       #xlab = "Fittede værdier" ,
       main = paste("Portefølje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}
### (16) Residual plots ---------------------------------------------

# Aktie model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(stock$date) == y & month(stock$date) == m 
  k <- length(stock$return[int])
  h <- 1/k * rep(1,k)
  
  famamod <- lm( stock$return[int] ~ h               +
                                     SPY$return[int] + 
                                     HML$return[int] + 
                                     SMB$return[int] -
                                     1)
  
  setEPS()
  postscript( paste("resid",snames[stockind],y,0,m,".eps",sep = "") ,
              height = 6)
  plot(famamod$residuals , ylab = "Residualer" , xlab = "Indeks" ,
       main = paste(snames[stockind]," - ",mns[m]," ",y,sep="" ) )
  dev.off()
}


# Portefoelje model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(naiveport$date) == y & month(naiveport$date) == m
  k <- length(naiveport$return[int])
  h <- 1/k * rep(1,k) 
  
  famamod <- lm( naiveport$return[int] ~ h               +
                                         SPY$return[int] + 
                                         HML$return[int] + 
                                         SMB$return[int] -
                                         1)
  setEPS()
  postscript( paste("residport",y,0,m,".eps", sep = "") , height = 6)
  plot(famamod$residuals , ylab = "Residualer" , xlab = "Indeks" ,
       main = paste("Portefølje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}
### (17) Residual hist mod norm -------------------------------------

y <- 2008
m <- 6
int <- year(naiveport$date) == y & month(naiveport$date) == m
k <- length(naiveport$return[int])
h <- 1/k * rep(1 , k)

famamod <- lm( naiveport$return[int]   ~ h               +
                                         SPY$return[int] + 
                                         HML$return[int] + 
                                         SMB$return[int] -
                                         1)
setEPS()
postscript( paste("residhistnorm",".eps",sep = "") , height = 6)

x <- seq(-0.006, 0.006, length=100)
hist(famamod$residuals , probability = TRUE , breaks = 100 ,
     xlim = c(-0.005 , 0.005) , 
     main = paste("Portefølje - ",mns[m]," ",y,sep="") ,
     xlab = "Residualer" , ylab = "Sandsynlighed")
lines(x , dnorm(x , mean = 0 , sd = 0.00076 ) ,lty = 2 , lwd = 1 )

dev.off()

### (18) Chi-i-anden plot med forskellige df ------------------------

x <- seq(0, 15, length=100)
degf <- c(3,5,7,10)

colors <- c("chartreuse4", 
            "royalblue4", 
            "firebrick4", 
            "darkorchid4")

labels <- c(paste("df = " , degf[1] , sep = "") ,
            paste("df = " , degf[2] , sep = "") ,
            paste("df = " , degf[3] , sep = "") ,
            expression(italic(df) == degf~4 ))

setEPS()
postscript( paste("chisqdist",".eps",sep = "") , height = 6)

plot(x, dchisq(x,degf[1]), type="l", col = colors[1], 
     xlab=expression(italic("x")), lwd=2 , 
     ylab=expression(italic("P(x)")), main="")

for (i in 2:length(degf)){
  lines(x, dchisq(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Frihedsgrader",
       labels, lwd=2, col=colors , bty="n")

dev.off()



### (19) t-fordeling plot med forskellige df ------------------------

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 20)
colors <- c("darkorchid4", 
            "firebrick4" ,
            "royalblue4" ,
            "chartreuse4",
            "black")

labels <- c(paste("df = " , degf[1] , sep = "") ,
            paste("df = " , degf[2] , sep = "") ,
            paste("df = " , degf[3] , sep = "") ,
            paste("df = " , degf[3] , sep = "") ,
            "normal")

setEPS()
postscript( paste("tnormdist",".eps",sep = "") , height = 6)

plot(x, hx, type="l", lty=2, xlab=expression(italic("x")),
     ylab=expression(italic("P(x)")), main="")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Fordelinger",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors , bty = "n")

dev.off()

### (20) F-fordeling plot med forskellige df ------------------------

x <- seq(0, 4, length=100)
degf1 <- c(3,5,7,10)
degf2 <- c(5,7,10,15)
colors <- c("chartreuse4", 
            "royalblue4", 
            "firebrick4", 
            "darkorchid4")

labels <- c(paste("df = ",degf1[1]," , ","df = ",degf2[1], sep = "") ,
            paste("df = ",degf1[2]," , ","df = ",degf2[2], sep = "") ,
            paste("df = ",degf1[3]," , ","df = ",degf2[3], sep = "") ,
            paste("df = ",degf1[4]," , ","df = ",degf2[4], sep = "") )

setEPS()
postscript( paste("fdist",".eps",sep = "") , height = 6)

plot(x, df(x,degf1[1],degf2[1]), type="l", col = colors[1], 
     xlab=expression(italic("x")), lwd=2 , 
     ylab=expression(italic("P(x)")), main="",ylim = c(0,0.85))

for (i in 2:length(degf)){
  lines(x, df(x,degf1[i],degf2[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Frihedsgrader",
       labels, lwd=2, col=colors , bty="n")

dev.off()


### (21) DET VILDE VESTEN -------------------------------------------

pfromsum <- c()
pfromwald <- c()
for(y in 2004){
  for(m in 6){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    famamodint   <- lm( naiveport$return[int]   ~ h               +
                                                  SPY$return[int] + 
                                                  HML$return[int] + 
                                                  SMB$return[int] -
                                                  1)
    famamodnoint <- lm( naiveport$return[int]   ~ SPY$return[int] + 
                                                  HML$return[int] + 
                                                  SMB$return[int] -
                                                  1)
    pfromsum  <- c(pfromsum , summary(famamodint)$coefficients[1,4])
    pfromwald <- c(pfromwald , 
                   waldtest(famamodint , famamodnoint)$`Pr(>F)`[2])
  }
}
summary(famamodint)
summary(famamodnoint)
waldtest(famamodint , famamodnoint)
waldtest(famamodnoint , famamodint)



for(y in 2008){ 
  for(m in 6){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    famamod   <- lm( naiveport$return[int]   ~ h               +
                                               SPY$return[int] + 
                                               HML$return[int] + 
                                               SMB$return[int] -
                                               1)
  }
}
summary(famamod)
dwtest(famamod)

T = summary(famamodint)$coefficients[1,1]/
    summary(famamodint)$coefficients[1,2]
pt(T , 1712)
