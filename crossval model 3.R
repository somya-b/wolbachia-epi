
##loading data
library(readxl)
source('funcs.R')
source('updated-data/names.R')

listData <- loadRData('updated-data/list-dengue.rdata')
listData <- lapply(listData, function(x) {x<-as.matrix(x)})
d1 <- read.csv('updated-data/dates.csv')
controls.identifier <- co

pop <- read.csv('updated-data/population.csv')

mapT <- data.frame(sno=1:444,time = rownames(listData$total), map=seq(1500,length.out=444))

listData <- lapply(listData, function(x) {rownames(x)<-mapT$map;x })
subData <- listData[10:35]

d1$Date <- sapply(d1$Date, function(x) {mapT$map[mapT$time ==x]})


library(doParallel)
n <- detectCores()
cl <- makeCluster(n-1)
registerDoParallel(cl, cores = n)


run.model <- function(i, cv ){
  treatment.identifier <- d1$Area[i]
  t <- d1$Date[i]-cv
  times.dep  <- cbind("subgrp"  = c(1500,t))
  times.pred <- cbind(times.dep,times.dep.func(1500,t))
  
  agg.fns <- rep("mean", ncol(times.pred))
  m3 <- mscmt(ldat, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose = F)
  
  return(m3)
}

## change here
subgroup = 'total'

ldat <- c(list(listData[[subgroup]]), subData)
ldat[1] <- lapply(ldat[1],function(x) normalize.pop(x,pop)) #normalizing incidence by total population in each year for all towns
names(ldat)[1] <- 'subgrp' 


out <- list()
intermediate_directory <- 'INTER_DIR/M3'
out <- foreach(cv=rep(1:52, each=4),i=rep(1:4,52), .packages = c('MSCMT','dplyr')) %dopar%{
  
  # Create a unique filename for each interation of the parallel loop
  each_filename <- paste0('RESULT_',as.character(i), '.rdata')
  each_folder <- paste0('CV_',as.character(cv))
  each_filepath <- file.path(intermediate_directory,each_folder,each_filename)
  
  # If the file exists, skip to the next iteration
  if (file.exists(each_filepath)) {next}
  
  # Otherwise, run your code
  each_result <- run.model(i=i, cv=cv)
  
  # Save the result individually
  save(each_result, file = each_filepath)
  
}

stopCluster(cl)



