### (1)  Introting ---------------------------------------------------

# Pakker
library(openxlsx)
library(lubridate)

# Mapper der bruges
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
cldatdir <- "./p5dataclean"

# Navne og placeringer på aktier
csvvec <- list.files(cldatdir , full.names = 1 , recursive = 0)
snames <- substr(basename(csvvec),1,nchar(basename(csvvec))-5)

### (2)  Sampling frekvens -------------------------------------------

n <- 5
priceseq <- seq(1,391,n)

### (3)  Specifik aktie indlæsning -----------------------------------
# Kør først (1) og (2)

# Valg af aktie
stockind   <- 1 ; print(csvvec[stockind])
dat <- read.csv(csvvec[stockind])

# (5 min)
pricevec <- c()
datevec  <- c()

for(i in 1:(length(dat) - 1) ){
  pricevec <- c(pricevec , dat[priceseq,(i+1)])
  datevec  <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
}
datevec <- ymd(substring(datevec,2))
stock   <- data.frame(date = datevec[2:length(datevec)] , 
                      price = pricevec[2:length(pricevec)] ,
                      return = diff(log(pricevec))  )
keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
stock <- stock[keep,]




# (1 min)
pricevec <- c()
datevec  <- c()

for(i in 1:(length(dat) - 1) ){
  pricevec <- c(pricevec , as.numeric(dat[1:391,(i+1)]) ) 
  datevec  <- c(datevec , rep(names(dat)[(i+1)],391) )
}
datevec <- ymd(substring(datevec,2))
stock1  <- data.frame(date = datevec , 
                      price = pricevec ,
                      return = c(0 , diff(log(pricevec)) ) )


### (4)  SPY indlæsning ----------------------------------------------
# Kør først (1) og (2)

# Valg af aktie
SPYind <- 28 ; print(csvvec[SPYind])
dat    <- read.csv(csvvec[SPYind])

# (5 min)
SPYprice <- c()
SPYdate  <- c()

for(i in 1:(length(dat) - 1) ){
  SPYprice <- c(SPYprice , dat[priceseq,(i+1)])
  SPYdate  <- c(SPYdate , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
}
SPYdate <- ymd(substring(SPYdate,2))
SPY     <- data.frame(date = SPYdate[2:length(SPYdate)] , 
                      price = SPYprice[2:length(SPYprice)] , 
                      return = diff(log(SPYprice))  )

keep <- (1:length(SPY[,1]))[!1:length(SPY[,1]) %in% ((1:2766)*79)]
SPY <- SPY[keep,]
 
# (1 min)
SPYprice <- c()
SPYdate  <- c()

for(i in 1:(length(dat) - 1) ){
  SPYprice <- c(SPYprice , as.numeric(dat[1:391,(i+1)]) ) 
  SPYdate  <- c(SPYdate , rep(names(dat)[(i+1)],391) )
}
SPYdate <- ymd(substring(SPYdate , 2))
SPY1    <- data.frame(date = SPYdate , 
                      price = SPYprice ,
                      return = c(0 , diff(log(SPYprice)) ) )

### (5)  HML og SMB indlæsning ---------------------------------------

# Mappe
factdir <- "./factors"
factcsv <- list.files(factdir , full.names = 1 , recursive = 0);factcsv

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

### (6)  Betatabel (hele perioden) -----------------------------------
# Kør først (1) , (2) , (4) og (5)

### Beta0 skæring , beta1 SPY , beta2 SMB , beta3 HML
betaframe <- data.frame(name   = snames ,
                        beta0  = rep(10,36) ,
                        beta1  = rep(10,36) ,
                        beta2  = rep(10,36) ,
                        beta3  = rep(10,36) ,
                        pbeta0 = rep(10,36)) ; head(betaframe)

for(k in 5:5){
  
  ### Priser og datoer for aktie k
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  datevec <- c()
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec,dat[priceseq,(i+1)])
    datevec <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
  }
  datevec <- ymd(substring(datevec,2))
  stock <- data.frame(date = datevec , 
                      price = pricevec ,
                      return = c(0 , diff(log(pricevec)) ) )
  
  ### Model
  ttime <- 1:length(stock$return) 
  famamod <- lm( stock$return[ttime] ~ SPY$return[ttime] + 
                                       HML$return[ttime] + 
                                       SMB$return[ttime])
  
  ### Udvaelger koefficienter til betaframe
  for(i  in 1:4){
    betaframe[k , i + 1] <- summary(mymodel)$coefficients[i,1]
  }
  betaframe[k , 6] <- summary(mymodel)$coefficients[1,4]
  print(k)
  print(datevec[1])
}

print(betaframe , digits = 3)

### (7)  P-vaerdi tabel (hele perioden) ------------------------------ 
# Kør først (1) , (2) , (4) og (5)

pframe <- data.frame(name    = snames ,
                     pbeta0  = rep(10,36) ,
                     pSPY    = rep(10,36) ,
                     pSMB    = rep(10,36) ,
                     pHML    = rep(10,36)) ; head(pframe)

for(k in 1:length(csvvec)){
  
  ### Priser og datoer for aktie k
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  datevec <- c()
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec,dat[priceseq,(i+1)])
    datevec <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
  }
  datevec <- ymd(substring(datevec,2))
  stock <- data.frame(date = datevec , 
                      price = pricevec ,
                      return = c(0 , diff(log(pricevec)) ) )
  
  ### Model
  ttime <- 1:length(stock$return) 
  famamod <- lm( stock$return[ttime] ~ SPY$return[ttime] + 
                                       HML$return[ttime] + 
                                       SMB$return[ttime])
  
  ### Udvaelger koefficienter til betaframe
  for(i  in 1:4){
    pframe[k , i + 1] <- summary(mymodel)$coefficients[i,4]
  }
  print(k)
}
 
print(pframe , digits = 5)

insignSPY <- snames[pframe$pSPY > 0.05] ; insignSPY
insignSMB <- snames[pframe$pSMB > 0.05] ; insignSMB
insignHML <- snames[pframe$pHML > 0.05] ; insignHML

### (8)  Naiv portefølje (5 min) -------------------------------------
# Kør først (1) og (2)

# Laver naiv portefølje data.frame
allstock <- data.frame(numeric(218513))
j <- 1
datevec <- c()

for(k in (1:36)[!1:36 %in% 28]){ # Aktier uden SPY
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , dat[priceseq , (i+1)] )
  }
  if(k == 1){ # Korteste aktie
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)],(391 + n - 1)/n))  
      
    }
    datevec <- ymd(substring(datevec,2))
  }
  
  allstock[,j] <- diff(log(pricevec))
  names(allstock)[j] <- snames[k]
  j <- j + 1
  print(k)
}
naiveport <- data.frame(date = datevec[2:218514] ,
                        return = (1/35) * rowSums(allstock) )
keep <- (1:length(naiveport[,1]))[!1:length(naiveport[,1]) %in% ((1:2766)*79)]
naiveport <- naiveport[keep,]

### (9)  CAPM --------------------------------------------------------
# Kør først (1) , (2) og (4)

# Forkorter SPY1 til at passe med korteste aktie
SPY1 <- SPY1[1:1081114,]

# Laver naiv portefølje data.frame
allstock <- data.frame(numeric(1081115)) # Længde af korteste aktie
j <- 1
datevec <- c()
for(k in (1:36)[!1:36 %in% 28]){ # Aktier uden SPY
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , as.numeric(dat[1:391,(i+1)]) )
  }
  if(k == 2){
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)],391) )  
    
      }
    datevec <- ymd(substring(datevec,2))
  }
  allstock[,j] <- pricevec[1:1081115]
  names(allstock)[j] <- snames[k]
  j <- j + 1
  print(k)
}
naiveport1 <- data.frame(date   = datevec[1:length(datevec)-1] ,
                         return = diff(log(rowSums(allstock))))

# CAPM model over hele perioden
capmmod <- lm(naiveport1$return ~ SPY1$return)
summary(capmmod)

betaSMB <- c()
beta0 <- c()
for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    #int <- c(int , rep(FALSE , 80))
    
    famamod <- lm( naiveport$return[int] ~ SPY$return[int] +
                                           HML$return[int] +
                                           SMB$return[int])
    
    betaSMB <- c(betaSMB , as.numeric(famamod$coefficients[4]) )
    beta0 <- c(beta0 , summary(famamod)$coefficients[1,4])
  }
}

### (10) R-adjusted CAPM vs Fama (5 min) ----------------------------
# Kør først (1) , (2) , (4) , (5) og (8)

rcapm <- rfama <- c()
rdate <- c("1999-01-04")

for(y in 1999:2009){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m 
    #int <- c(int , rep(FALSE , 80))
    
    capmmod <- lm(naiveport$return[int] ~ SPY$return[int])
    
    famamod <- lm(naiveport$return[int] ~ SPY$return[int] +
                                          HML$return[int] +
                                          SMB$return[int])
    rcapm <- c(rcapm , summary(capmmod)$adj.r.squared)
    rfama <- c(rfama , summary(famamod)$adj.r.squared)
    rdate <- c(rdate , (naiveport$date[int])[1] ) # VIRKER IKKE FUCK R
  }
}


plot(rfama - rcapm , type = "l" , 
                     col = "blue" ,
                     ylim = c(-0.01 , 0.03) ,
                     xlab = "Måneder efter 1999-01" ,
                     ylab = "R^2 Differens (Fama - CAPM)")
abline(h = 0)

### (11) Residual plots ---------------------------------------------
# Kør først (1) , (2) , (4) , (5) og (8)
mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

# Aktie model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(stock$date) == y & month(stock$date) == m 
  #int <- c(int , rep(FALSE , 70000))
  
  k <- length(stock$return[int])
  ds <- length( unique( day( stock$date[int] )  ) )
  h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
  
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


# Portefølje model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(naiveport$date) == y & month(naiveport$date) == m
  #int <- c(int , rep(FALSE , 70000)) # Fordi porteføljen er kortere
    
  k <- length(naiveport$return[int])
  ds <- length( unique( day( naiveport$date[int] )  ) )
  h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
    
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
       main = paste("Naiv portefølje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}

### (12) R^2 tabel --------------------------------------------------

radjframe <- data.frame(X2004 = rep(0,36) , 
                        X2008 = rep(0,36) , 
                        Gns   = rep(0,36))
row.names(radjframe) <- c(snames[(1:36)[!1:36 %in% 28]] , "Port")
j <- 1
for(stockind in (1:36)[!1:36 %in% 28]){
  
  radj <- c()
  
  dat <- read.csv(csvvec[stockind])
 
  pricevec <- c()
  datevec  <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , dat[priceseq,(i+1)])
    datevec  <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
  }
  datevec <- ymd(substring(datevec,2))
  stock   <- data.frame(date = datevec[2:length(datevec)] , 
                        price = pricevec[2:length(pricevec)] ,
                        return = diff(log(pricevec))  )
  keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
  stock <- stock[keep,]

  for(y in 1999:2009 ){
    for(m in 1:12){
      int <- year(stock$date) == y & month(stock$date) == m 
      #int <- c(int , rep(FALSE , 70000))
    
      k <- length(stock$return[int])
      ds <- length( unique( day( stock$date[int] )  ) )
      h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
    
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
    #int <- c(int , rep(FALSE , 70000)) # Fordi porteføljen er kortere
    
    k <- length(naiveport$return[int])
    ds <- length( unique( day( naiveport$date[int] )  ) )
    h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (391 * dage)) 
    
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

### (13) Skedastitetsplots ------------------------------------------
# Kør først (1) , (2) , (3) , (4) , (5) og (8)
mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

# Aktie model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(stock$date) == y & month(stock$date) == m 
  #int <- c(int , rep(FALSE , 70000))
  
  k <- length(stock$return[int])
  ds <- length( unique( day( stock$date[int] )  ) )
  h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
  
  famamod <- lm( stock$return[int] ~ h               +
                   SPY$return[int] + 
                   HML$return[int] + 
                   SMB$return[int] -
                   1)
  
  #setEPS()
  #postscript( paste("skedaresid",snames[stockind],y,0,m,".eps",sep = "") ,
  #            height = 6)
  plot(famamod , 
       which = 3 ,             # Skedastisitetsplot
       sub.caption = "" ,      # Ingen sub.caption
       #xlab = "Fittede værdier" ,
       main = paste(snames[stockind]," - ",mns[m]," ",y,sep="" ) )
  #dev.off()
}


# Portefølje model
for(y in c(2004 , 2008) ){
  m <- 6
  int <- year(naiveport$date) == y & month(naiveport$date) == m
  #int <- c(int , rep(FALSE , 70000)) # Fordi porteføljen er kortere
  
  k <- length(naiveport$return[int])
  ds <- length( unique( day( naiveport$date[int] )  ) )
  h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
  
  famamod <- lm( naiveport$return[int] ~ h               +
                   SPY$return[int] + 
                   HML$return[int] + 
                   SMB$return[int] -
                   1)
  setEPS()
  postscript( paste("skedaresidport",y,0,m,".eps", sep = "") , height = 6)
  plot(famamod , 
       which = 3 ,             # Skedastisitetsplot
       sub.caption = "" ,      # Ingen sub.caption
       #xlab = "Fittede værdier" ,
       main = paste("Naiv portefølje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}
### (14) Cor mellem faktorer og residualer --------------------------
# Kør først (1) , (2) , (3) , (4) , (5) og (8)
corframe <- data.frame(SPY = numeric(9) , 
                       HML = numeric(9) ,
                       SMB = numeric(9))
row.names(corframe)[7:9] <- c("Portefølje 2004" , "Portefølje 2008" ,
                              "Portefølje Gns")

# Aktie 1
stockind   <- 7 ; print(csvvec[stockind])
dat <- read.csv(csvvec[stockind])
pricevec <- c()
datevec  <- c()

for(i in 1:(length(dat) - 1) ){
  pricevec <- c(pricevec , dat[priceseq,(i+1)])
  datevec  <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
}
datevec <- ymd(substring(datevec,2))
stock   <- data.frame(date = datevec[2:length(datevec)] , 
                      price = pricevec[2:length(pricevec)] ,
                      return = diff(log(pricevec))  )
keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
stock <- stock[keep,]
row.names(corframe)[1:3] <- c(paste(snames[stockind] ," 2004" , sep = "") ,
                              paste(snames[stockind] ," 2008" , sep = "") , 
                              paste(snames[stockind] ," Gns" , sep =""))

j <- 1
cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m 
    #int <- c(int , rep(FALSE , 70000))
    
    k <- length(stock$return[int])
    ds <- length( unique( day( stock$date[int] )  ) )
    h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
    
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
dat <- read.csv(csvvec[stockind])
pricevec <- c()
datevec  <- c()

for(i in 1:(length(dat) - 1) ){
  pricevec <- c(pricevec , dat[priceseq,(i+1)])
  datevec  <- c(datevec , rep(names(dat)[(i+1)],(391 + n - 1)/n) )
}
datevec <- ymd(substring(datevec,2))
stock   <- data.frame(date = datevec[2:length(datevec)] , 
                      price = pricevec[2:length(pricevec)] ,
                      return = diff(log(pricevec))  )
keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
stock <- stock[keep,]
row.names(corframe)[1:3] <- c(paste(snames[stockind] ," 2004" , sep = "") ,
                              paste(snames[stockind] ," 2008" , sep = "") , 
                              paste(snames[stockind] ," Gns" , sep =""))

cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(stock$date) == y & month(stock$date) == m 
    #int <- c(int , rep(FALSE , 70000))
    
    k <- length(stock$return[int])
    ds <- length( unique( day( stock$date[int] )  ) )
    h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
    
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

# Portefølje
cormeanSPY <- c()
cormeanHML <- c()
cormeanSMB <- c()
for(y in 1999:2009 ){
  for(m in 1:12){
    int <- year(naiveport$date) == y & month(naiveport$date) == m 
    #int <- c(int , rep(FALSE , 70000))
    
    k <- length(naiveport$return[int])
    ds <- length( unique( day( naiveport$date[int] )  ) )
    h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (78 * dage)) 
    
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

### (15) TEST -------------------------------------------------------
betaSMB <- c()
beta0 <- c()
radjstock <- c()
for(y in 1999){
  for(m in 1){
    int <- year(stock$date) == y & month(stock$date) == m 
    #int <- c(int , rep(FALSE , 70000))
    k <- length(stock$return[int])
    ds <- length( unique( day( stock$date[int] )  ) )
    h <- 1/(391*ds) * rep(1,k) # 1/(antal obs på måneden (391 * dage)) 
    famamod <- lm( stock$return[int] ~ SPY$return[int] + 
                                       HML$return[int] + 
                                       SMB$return[int])
    
    betaSMB <- c(betaSMB , as.numeric(famamod$coefficients[4]) )
    beta0 <- c(beta0 , summary(famamod)$coefficients[1,4])
    radjstock <- c(radjstock , summary(famamod)$adj.r.squared)
  }
}


pframe <- data.frame(pbeta0  = rep(10,132) ,
                     pSPY    = rep(10,132) ,
                     pHML    = rep(10,132) ,
                     pSMB    = rep(10,132)) ; head(pframe)
radjport <- c()
ppos <- 1
for(y in 1999){
  for(m in 1){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    #int <- c(int , rep(FALSE , 70000)) # Fordi porteføljen er kortere
    
    k <- length(naiveport$return[int])
    ds <- length( unique( day( naiveport$date[int] )  ) )
    h <- 1/(78*ds) * rep(1,k) # 1/(antal obs på måneden (391 * dage)) 
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)

    pframe[ppos,] <- as.numeric(summary(famamod)$coefficients[,4])
    ppos <- ppos + 1
    radjport <- c(radjport , summary(famamod)$adj.r.squared )
    print(m)
    print(summary(famamod))
    }
  print(y)
}

signbeta0 <- pframe$pbeta0[pframe$pbeta0 < 0.05] ; length(signbeta0)
insignSPY <- pframe$pSPY[pframe$pSPY > 0.05] ; length(insignSPY)
insignSMB <- pframe$pSMB[pframe$pSMB > 0.05] ; length(insignSMB)
insignHML <- pframe$pHML[pframe$pHML > 0.05] ; length(insignHML)
insignALL <- pframe$pSPY[pframe$pSPY > 0.05 &
                         pframe$pSMB > 0.05 &
                         pframe$pHML > 0.05] ; length(insignALL)
summary(famamod)
plot(famamod$residuals , pch = 20 , cex = 0.7 , col = "blue")


plot(radjport , type = "l" , col = "blue" , ylim = c(0,1))
lines(radjstock , type = "l" , col = "red")
plot(radjport - radjstock , type = "l")
