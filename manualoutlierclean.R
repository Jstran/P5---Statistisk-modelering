library(openxlsx)
setwd("C:/Users/jonat/Desktop/R Kode/P5/P5---Statistisk-modelering")
cldatdir <- "./p5dataclean"
csvvec <- list.files(cldatdir , full.names = 1 , recursive = 0)
  sekvens <- seq(1,391,5)
  
  # for(k in 1:length(pricevec))
  k <- 7 ; csvvec[k]
  dat <- read.csv(csvvec[k])
  pricevec <- c()
  datevec <- c()
    for(i in 1:(length(dat) - 1) ){
      pricevec <- c(pricevec,dat[,(i+1)])
      datevec <- c(datevec , rep(names(dat)[(i+1)],391) )
    }
  # pdf(paste("plot",k,".pdf"))
  plot(pricevec , type = "l", main = basename(csvvec[k]))
  # dev.off()
  # }
interval <- 150000:550000
plot(pricevec[interval] , type = "l", 
                          main = basename(csvvec[k]) ,
                          col = "black")

## Minimum
wrongplace <- which.min(pricevec[interval])
min(pricevec[interval])

## Maximum
wrongplace <- which.max(pricevec[interval]);wrongplace
max(pricevec[interval])

## Dato
(datevec[interval])[wrongplace]
csvvec[k]  
  
factors <- list.files("./factors",full.names = 1 , recursive = 0)
faktorHML <- read.xlsx(factors[1])
faktorSMB <- read.xlsx(factors[2])

shift <- 505000 ; datevec[shift]
n <- 30 # Sampling frequency
RV <- c()
for(j in n:1){
  RV[n + 1 - j] <- sum( diff( log( pricevec[seq(shift, shift + 5*391,j)] ) )^2 )
}


#setEPS()
#postscript("realiseretvol.eps", height = 4)
plot((n:1) , RV , 
     xlab = "Observationsfrekvens" ,
     ylab = "Realiseret volatilitet")
#dev.off()


plot(pricevec , type = "l")



csvvec <- list.files("./p5data/ALL" , full.names = 1 , recursive = 0)
test <- read.csv("./p5data/ALL/ALL_20031215.csv" , sep = "," , dec = ".")
max(test$price)
# 1230:1270
test <- c()
for(k in 1229:1229){
  testframe <- read.csv(csvvec[k])
  test <- c(test,testframe$price)
}
max(test)
which.max(test)
length(test)
test[220520:220522]
