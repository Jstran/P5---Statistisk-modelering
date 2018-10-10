library(lubridate)
library(tidyverse)
library(microbenchmark)

hmscol <- hms(c("09:38:58" , "09:38:57")) ; hmscol
tiddeci <- hour(hmscol)*60 + minute(hmscol) + second(hmscol)/60 ; tiddeci

microbenchmark(
  
  hour(hmscol[1])   == hour(hmscol[2]),
  minute(hmscol[1]) == minute(hmscol[2]),
  second(hmscol[1]) == second(hmscol[2]),
  
  hour(hmscol[1])   <  hour(hmscol[2]),
  minute(hmscol[1]) <  minute(hmscol[2]),
  second(hmscol[1]) <  second(hmscol[2]),
  
  tiddeci[1]        == tiddeci[2],
  tiddeci[1]        <  tiddeci[2],
  unit = "ms"
)

