## load libraries and data
library(MSCMT)
library(data.table)
source('updated-data/names.R')
source('funcs.R')

# list of matrices containg all dengue incidence and characteristics data
listData <- loadRData('updated-data/list-dengue.rdata')
listData <- lapply(listData, function(x) {x<-as.matrix(x)})

# file containing intervention start dates
d1 <- read.csv('updated-data/dates.csv')

controls.identifier <- co

# file containing town population
pop <- read.csv('updated-data/population.csv')

# maping e-week year to serial numbers to put into function
mapT <- data.frame(sno=1:444,time = rownames(listData$total), map=seq(1500,length.out=444))

listData <- lapply(listData, function(x) {rownames(x)<-mapT$map;x })
subData <- listData[10:35]  # contains covariates data

d1$Date <- sapply(d1$Date, function(x) {mapT$map[mapT$time ==x]})


model2 <- function(subgrp){
  res <- vector('list',nrow(d1))
  names(res) <- d1$Area
  for (i in 1:nrow(d1)) {
    treatment.identifier <- d1$Area[i]
    t <- d1$Date[i]
    times.dep  <- cbind("subgrp"  = c(1500,t))
    times.pred <- cbind(times.dep,times.dep.func(1500,t))
    
    agg.fns <- rep("last", ncol(times.pred))
    m2 <- mscmt(data, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose = F)
    
    res[[i]] <- m2
  }
  
  return(res)
}

## change here 
subgroup = 'total'

ldat <- c(list(listData[[subgroup]]), subData)
ldat[1] <- lapply(ldat[1],function(x) normalize.pop(x,pop)) #normalizing incidence by town populations
names(ldat)[1] <- 'subgrp' 
out <- model2(ldat)
save(out,file=paste0('model-out/m2-',subgroup,'.rdata') )



