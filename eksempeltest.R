test <- c(1,2,3,4)
newvariable <- test + 2

for (i in 1:4){
  newvariable[i] <- newvariable[i] + i
}

newvariable