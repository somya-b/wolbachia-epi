#load libraries and data
library(MSCMT)
library(data.table)
source('funcs.R')
source('updated-data/names.R') #contains names of release and control towns

listData <- loadRData('updated-data/list-dengue.rdata')
listData <- lapply(listData, function(x) {x<-as.matrix(x)})
d1 <- read.csv('updated-data/dates.csv')

pop <- read.csv('updated-data/population.csv')

mapT <- data.frame(sno=1:444,time = rownames(listData$total), map=seq(1500,length.out=444))

#mapping e-week year to sequential numbers to be used in model function
listData <- lapply(listData, function(x) {rownames(x)<-mapT$map;x })
subData <- listData[10:35] #extracting covariate data
d1$Date <- sapply(d1$Date, function(x) {mapT$map[mapT$time ==x]})

#model function to generate synthetic control for donor group different different models
# @subgrp : subgroup of cases
# @cv : validation period 
# @m : model no. based on aggregation function used; 4-no covariates, 3-average of pre-intervention outcome, 2-last pre-intervention outcome
model <- function(subgrp, m){
  res <- vector('list',length(co))
  names(res) <- co
  for (i in 1:length(co)){
    
    t <- d1$Date[4] #taking treatment start date as earliest start date for all towns in donor group
    times.dep  <- cbind("subgrp"  = c(1560,t))
    treatment.identifier <- co[i] 
    controls.identifier <- co[!co %in% c(treatment.identifier)]
    
    if (m==4){
      times.pred <- cbind("subgrp"  = c(1560,t))
      m3 <-  mscmt(subgrp, treatment.identifier, controls.identifier, times.dep, times.pred,verbose = F)
    } else if (m==3){
      times.pred <- cbind(times.dep,times.dep.func(t))
      agg.fns <- rep("mean", ncol(times.pred)) 
      m3 <-  mscmt(subgrp, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose=F)
    } else if (m==2){
      times.pred <- cbind(times.dep,times.dep.func(t))
      agg.fns <- rep("last", ncol(times.pred)) 
      m3 <-  mscmt(subgrp, treatment.identifier, controls.identifier, times.dep, times.pred, agg.fns, verbose=F)
    }
    
    res[[i]] <- m3
  }
  return(res)
}




## for model 4
total <- c(list(listData$total))
total[1] <- lapply(total[1],function(x) normalize.pop(x,pop)) #normalizing incidence by total population in each year for all towns
names(total)[1] <- 'subgrp' 

m4 <- model(total,4)
save(m4,file='control-m4.rdata')




total <- c(list(listData$total), subData)
total[1] <- lapply(total[1],function(x) normalize.pop(x,pop)) #normalizing incidence by total population in each year for all towns
names(total)[1] <- 'subgrp' 

## for model 3
m3 <- model(total,3)
save(m3,file='control-m3.rdata')

## for model 2
m2 <- model(total,2)
save(m2,file='control-m2.rdata')




m=3
res <- loadRData(paste0('control-',m,'.rdata'))


# calculating aggregate intervention efficacy of donor group
ie <- data.frame(pa=names(res), ie=NA)

ie$ie <- NA
for (i in 1:30) {
  t <- d1$Date[4]
  jj <- which(mapT$map==t)
  gaps <- res[[i]]$combined$subgrp[-c(1:jj),3]
  sc <- res[[i]]$combined$subgrp[-c(1:jj),2]
  
  ie$ie[i]= -100*mean(gaps)/mean(sc)
}

View(ie)

# performing Andrews' test on donor group
sc <- c(); real <- c()
for(i in 1:4){
  sc <- cbind(sc, res[[i]]$combined$subgrp[,2])
  real <- cbind(real, res[[i]]$combined$subgrp[,1]) 
}
colnames(sc)<- colnames(real)<-names(res)

dd <- data.frame(pa=colnames(sc), pval=NA)

for (i in 1:30) {
  t <- d1$Date[4]
  S_t <- real[,i]-sc[,i]
  jj <- which(map$now==t)
  
  dd$pval[i]=Andrews.test(S_t, M = jj)$p.value
}
View(dd)


