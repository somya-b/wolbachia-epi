
#### run scm model ####
##loading data
library(stringi)
library(readxl)
source('../funcs.R')
source('../updated-data/names.R')

listData <- loadRData('../updated-data/list-dengue.rdata')
listData <- lapply(listData, function(x) {x<-as.matrix(x)})
d1 <- read.csv('../updated-data/dates.csv')
controls.identifier <- co

pop <- read.csv('../updated-data/population.csv')

mapT <- data.frame(sno=1:444,time = rownames(listData$total), map=seq(1500,length.out=444))

listData <- lapply(listData, function(x) {rownames(x)<-mapT$map;x })
subData <- listData[10:35] # contains covariates data

d1$Date <- sapply(d1$Date, function(x) {mapT$map[mapT$time ==x]})

bt.data <- loadRData(paste0('bb-',sungroup,'.rdata') )
bt.data <- lapply(bt.data, function(x) normalize.pop(x,pop))
bt.data <- lapply(bt.data, function(x) {rownames(x)<-c(1500:1943); x})


library(doParallel)
n <- detectCores()
cl <- makeCluster(n-1)
registerDoParallel(cl, cores = n)


run.model <- function(data,i){
  treatment.identifier <- d1$Area[i]
  t <- d1$Date[i]
  times.dep  <- cbind("subgrp"  = c(1500,t))
  times.pred <- cbind(times.dep,times.dep.func(1500,t))
  
  agg.fns <- rep("last", ncol(times.pred))
  m2 <- mscmt(data, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose = F)
  
  return(m2)
}

out <- list()


intermediate_directory <- paste0('BOOT/M2/',stri_trans_toupper(subgroup))

out <- foreach(id=rep(1:100, each=4),i=rep(1:4,100), .packages = c('MSCMT','dplyr','data.table')) %dopar%{
  
  # Create a unique filename for each iteration of the parallel loop
  each_filename <- paste0('RESULT_',as.character(i), '.rdata') #bootstrap file name
  each_folder <- paste0('B_',as.character(id)) # bootstrap iteration folder
  each_filepath <- file.path(intermediate_directory,each_folder,each_filename)
  
  # If the file exists, skip to the next iteration
  if (file.exists(each_filepath)) {next}
  
  # Otherwise, run your code
  ldat <- c(list(as.matrix(bt.data[[id]])), subData) 
  names(ldat)[1] <- 'subgrp' 
  each_result <- run.model(data=ldat, i=i)
  
  # Save the result individually
  save(each_result, file = each_filepath)
}

stopCluster(cl)



