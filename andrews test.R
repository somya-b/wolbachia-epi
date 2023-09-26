##load libraries
library(tseries)
library(CPAT)
source('funcs.R')
source('updated-data/names.R')

#### ---- change here -----
subgrp = 'total'
model = 3

res <- loadRData(paste0('model-out/m',model,'-',subgrp,'.rdata') )


#calculate Andrews' p-value for study sites 
for (i in 1:4) {
  print(i)
  t <- d1$Date[i] #treatment start date
  S_t <- res[[i]]$combined$subgrp[,3]  #gives gap at each time-point for i'th site
  jj <- which(map$now==t)
  ts <- S_t[1:jj]
  print( adf.test(ts) ) #do stationarity test for pre-intervention gap
  
  hh<-Andrews.test(S_t, M = jj) #Andrews' test for gap with time-point of change at treatment start date
  print(hh)
}



