### (1)  Introting ---------------------------------------------------

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

### (2)  Sampling frekvens -------------------------------------------

priceseq <- seq(1,391,5)

### (3)  stockreader funktion ---------------------------------------

# Funktion der indlaeser aktier nr 'stockind' i csvvec, og indsaetter
# det i data.frame (vektor hvis onlyreturn = TRUE), hvori overnight
# returns er fjernet. Det er for 5 min afkast
stockreader <- function(stockind , onlyreturn = FALSE){
  
  pricevec <- c()
  if(onlyreturn == FALSE){
    dat <- read.csv(csvvec[stockind])
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
    dat <- read.csv(csvvec[stockind])
    
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

### (8)  Naiv portefoelje (5 min) -------------------------------------

# Laver naiv portefoelje data.frame
allstock <- data.frame(numeric(218513))
j <- 1
datevec <- c()

for(k in (1:36)[!1:36 %in% 28]){ # Aktier uden SPY
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , dat[priceseq , (i+1)] )
  }
  if(k == 1){
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)], 79))  
      
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



# Laver naiv portefoelje data.frame
allstock <- data.frame(numeric(218513))
j <- 1
datevec <- c()

for(k in (1:36)[!1:36 %in% 28]){ # Aktier uden SPY
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  
  for(i in 1:(length(dat) - 1) ){
    pricevec <- c(pricevec , dat[priceseq , (i+1)] )
  }
  if(k == 1){
    for(h in 1:(length(dat) - 1)){
      datevec  <- c(datevec , rep(names(dat)[(h+1)], 79))  
      
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



















### (11) QQ residual plots ------------------------------------------

mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

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

### (12) R^2 tabel --------------------------------------------------

radjframe <- data.frame(X2004 = rep(0,36) , 
                        X2008 = rep(0,36) , 
                        Gns   = rep(0,36))
row.names(radjframe) <- c(snames[-28] , "Port")

j <- 1
for(stockind in (1:length(snames))[-28]){
  
  radj <- c()
  
  dat <- read.csv(csvvec[stockind])
 
  pricevec <- c()
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

### (13) Skedastitetsplots ------------------------------------------

mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

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
       which = 3 ,             # Skedastisitetsplot
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
       which = 3 ,             # Skedastisitetsplot
       sub.caption = "" ,      # Ingen sub.caption
       #xlab = "Fittede værdier" ,
       main = paste("Naiv portefølje - ",mns[m]," ",y,sep="" ) ) 
  dev.off()
}
### (14) Cor mellem faktorer og residualer --------------------------

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
  datevec  <- c(datevec , rep(names(dat)[(i+1)], 79) )
}
datevec <- ymd(substring(datevec,2))
stock   <- data.frame(date = datevec[2:length(datevec)] , 
                      price = pricevec[2:length(pricevec)] ,
                      return = diff(log(pricevec))  )
keep <- (1:length(stock[,1]))[!1:length(stock[,1]) %in% ((1:2766)*79)]
stock <- stock[keep,]
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
dat <- read.csv(csvvec[stockind])
pricevec <- c()
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

### (15) Residual plots ---------------------------------------------

mns <- c("januar" , "februar" , "marts" , "april" , "maj" ,
         "juni" , "juli" , "august" , "september" , "oktober" ,
         "november" , "december")

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
  h <- 1/k * rep(1,k) # 1/(antal obs paa maaneden (78 * dage)) 
  
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
### (16) Breusch-Pagan tests ----------------------------------------

bppframe <- data.frame(Pleq = numeric(3) , 
                       Pg   = numeric(3))
row.names(bppframe)[3] <- "Portefølje"

# Aktie 1
stockind   <- 7 ; print(csvvec[stockind])
dat <- read.csv(csvvec[stockind])
pricevec <- c()
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
dat <- read.csv(csvvec[stockind])
pricevec <- c()
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

### (17) R^2 Fama v CAPM5 v CAPM1 -----------------------------------

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

### (18) DET VILDE VESTEN -------------------------------------------

betaSMB <- c()
beta0 <- c()
radjstock <- c()
for(y in 1999){
  for(m in 1){
    int <- year(stock$date) == y & month(stock$date) == m 
    
    k <- length(stock$return[int])
    ds <- length( unique( day( stock$date[int] )  ) )
    h <- 1/(391*ds) * rep(1,k) # 1/(antal obs paa maaneden (391 * dage)) 
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
bpport <- c()
ppos <- 1
for(y in 2004){
  for(m in 6){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    
    
    k <- length(naiveport$return[int])
    h <- 1/(78*ds) * rep(1,k) 
    
    famamod <- lm( naiveport$return[int] ~ h               +
                                           SPY$return[int] + 
                                           HML$return[int] + 
                                           SMB$return[int] -
                                           1)

    pframe[ppos,] <- as.numeric(summary(famamod)$coefficients[,4])
    ppos <- ppos + 1
    radjport <- c(radjport , summary(famamod)$adj.r.squared )
    #print(m)
    bpport <- c(bpport , bptest(famamod)$p.value)
    }
  print(y)
}

plot(bpport , col = "red")
abline( h = 0.05 )
points((1:132)[bpport > 0.05] , bpport[bpport > 0.05] , col = "blue")


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




for(y in 2005){
  for(m in 1){
    int <- year(naiveport$date) == y & month(naiveport$date) == m
    k <- length(naiveport$return[int])
    h <- 1/k * rep(1 , k)
    
    famamodint <- lm( naiveport$return[int]   ~ h               +
                                                SPY$return[int] + 
                                                HML$return[int] + 
                                                SMB$return[int] -
                                                1)
    famamodnoint <- lm( naiveport$return[int] ~ SPY$return[int] +
                                                HML$return[int] + 
                                                SMB$return[int] -
                                                1)
    
    anova(famamodint , famamodnoint)
  }
}
plot(famamodint , which = 4)
plot(famamodint , which = 5)
plot(famamodint , which = 6)



y <- 2
testfunc <- function(x){
  x <- x + y
  return(x)
}
testfunc(4)
