library(lubridate)
library(tidyverse)
library(microbenchmark)

hmscol <- hms(c("09:38:58" , "09:38:57")) ; hmscol
tiddeci <- hour(hmscol)*60 + minute(hmscol) + second(hmscol)/60 ; tiddeci
mincol <- minute(hmscol)

microbenchmark(
  
  hour(hmscol[1])   == hour(hmscol[2]),
  minute(hmscol[1]) == minute(hmscol[2]),
  second(hmscol[1]) == second(hmscol[2]),
  
  hour(hmscol[1])   <  hour(hmscol[2]),
  minute(hmscol[1]) <  minute(hmscol[2]),
  second(hmscol[1]) <  second(hmscol[2]),
  
  tiddeci[1]        == tiddeci[2],
  tiddeci[1]        <  tiddeci[2],
  
  mincol[1]         == mincol[2],
  mincol[1]         <  mincol[2],
  unit = "ms"
)
# Hvis vi skal bruge minute() og sådan, skal der vektorene bare laves på forhånd. Så er evalueringerne hurtige

