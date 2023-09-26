library(dplyr)
source('funcs.R')
source('updated-data/names.R')

#### ---- change here -----
model = 3
subgrp = 'total'

res <- loadRData(paste0('model-out/m',model,'-',subgrp,'.rdata') )

sc <- c(); real <- c()
for(i in 1:4){
  sc <- cbind(sc, res[[i]]$combined$subgrp[,2])
  real <- cbind(real, res[[i]]$combined$subgrp[,1]) 
}
colnames(sc)<- colnames(real)<-names(res)

sc <- as.data.frame(sc); real <- as.data.frame(real)

#removing Q1 2019 to Q2 2020 for Bukit Batok and Choa Chu Kang since they didn't experience release until Q3 2020
d1 <- read.csv('updated-data/dates.csv')
mapT <- read.csv('updated-data/mapT.csv')

ii <- mapT$ey.ew < 2020.23 # for bukit batok
sc[ii,1] <- real[ii,1] <- NA
ii <- mapT$ey.ew < 2020.20 # for choa chu kang
sc[ii,2] <- real[ii,2] <- NA
sc$yr <- real$yr <- floor(mapT$ey.ew)

sc.t <- apply(sc[,1:4],1,function(x)sum(x,na.rm=T))
real.t <- apply(real[,1:4],1,function(x)sum(x,na.rm=T))
sc.t <- data.frame(yr=sc$yr, sc=sc.t)
real.t <- data.frame(yr=real$yr,real=real.t)

#### ---- Intervention Efficacy ----
sc.yr <- sc.t %>% group_by(yr) %>% summarise_all(sum)
real.yr <- real.t %>% group_by(yr) %>% summarise_all(sum)
ie.agg <- cbind(sc.yr[6:9,1],100*(sc.yr$sc[6:9]-real.yr$real[6:9])/sc.yr$sc[6:9])
ie.agg 

#summarizing by year
sc2 <- sc %>% group_by(yr) %>% summarise_all(function(x)sum(x,na.rm=T))
real2 <- real %>% group_by(yr) %>% summarise_all(function(x)sum(x,na.rm=T))

ie <- 100*(sc2[2:5]-real2[2:5])/sc2[2:5]
ie <- cbind(sc2[6:9,1],ie[6:9,])

### ----- calculate CIs ------
bb  <- loadRData(paste0('bootstrap/m',model,'-',subgrp,'.rdata') )
bounds <- getCI(sc[,1:4],bb)
ub <- bounds$ub
lb <- bounds$lb
li <- lb<0
lb[li] = 0

bb.bounds <- data.frame(yrs=qyr,lb=lb,ub=ub)
ii <- bb.bounds[,1]<2020.3 & bb.bounds[,1] > 2019 #remove Q1 2019 to Q2 2020 for Bukit Batok and Choa Chu Kang
bb.bounds[ii,c(2:3,6:7)] <- NA
bb.bounds[,1] <- floor(bb.bounds[,1])

bb.eff <- bb.bounds %>% group_by(yrs) %>% summarise_all(function(x) sum(x, na.rm = T))

#CIs for total IE
lb.t <- apply(bb.eff[,2:5],1,sum)
ub.t <- apply(bb.eff[,6:9],1,sum) 
lb.alpha <- -100*(real.yr[6:9,2]-lb.t[6:9])/lb.t[6:9]
ub.alpha <- -100*(real.yr[6:9,2]-ub.t[6:9])/ub.t[6:9]

mod <- data.frame(yrs= ie.agg[,1], alpha=NA)
for (i in 1:nrow(mod)) {
  mod[i,-1] <- paste0(round(ie.agg[i,-1],2)," (",round(lb.alpha[i,1],2)," – ",round(ub.alpha[i,1],2),")")
}

# CIs for town-wise IEs
diff.lb <- diff.ub <- data.frame(yrs=bb.eff[6:9,1],bb=NA, cck=NA,tp=NA,ys=NA)
for (i in 2:5) {
  diff.lb[,i] <-  -100*(real2[6:9,i]-bb.eff[6:9,i] )/bb.eff[6:9,i]
  diff.ub[,i] <-  -100*(real2[6:9,i]-bb.eff[6:9,i+4] )/bb.eff[6:9,i+4]
}
diff.lb[,-1] <- round(diff.lb[,-1],2)
diff.ub[,-1] <- round(diff.ub[,-1],2)

CIs <- data.frame(yr=ie[,1], bb=NA, cck=NA, tp=NA, ys=NA)
for (i in 1:nrow(ie)) {
  CIs[i,-1] <- paste0(round(ie[i,-1],2)," (",diff.lb[i,-1]," – ",diff.ub[i,-1],")")
}

View(cbind(mod,CIs[,-1]))


### ---- reduction per 100,000 persons ----
sum(sc.yr[6:9,2])*100000-sum(real.yr[6:9,2])*100000
sum(lb.t[6:9])*100000-sum(real.yr[6:9,2])*100000
sum(ub.t[6:9])*100000-sum(real.yr[6:9,2])*100000

### --- cases averted ----
pop <- read.csv('updated-data/population.csv')
study.pop <- pop[pop$PA %in% st,]
sc.pop <- real.pop <- lb.pop <- ub.pop <- data.frame(yr=sc2$yr, bb=NA, cck=NA, tp=NA, ys=NA)
for (i in 2:5) {
  sc.pop[,i] <- sc2[,i]*study.pop[i-1,2]
  real.pop[,i] <- real2[,i]*study.pop[i-1,2]
  lb.pop[,i] <- bb.eff[,i]*study.pop[i-1,2]
  ub.pop[,i] <- bb.eff[,i+4]*study.pop[i-1,2]
}
agg.ca <- apply(sc.pop[,2:5],1,sum)-apply(real.pop[,2:5],1,sum)
agg.lb <- apply(lb.pop[,2:5],1,sum)-apply(real.pop[,2:5],1,sum)
agg.ub <- apply(ub.pop[,2:5],1,sum)-apply(real.pop[,2:5],1,sum)
out.agg <- data.frame(yrs= sc.pop[6:9,1], agg=NA)
for (i in 1:nrow(out.agg)) {
  out.agg[i,-1] <- paste0(round(agg.ca[i+5],2)," (",round(agg.lb[i+5],2)," – ",round(agg.ub[i+5],2),")")
}

ca <- sc.pop[6:9,2:5]-real.pop[6:9,2:5]
lb.ca <- lb.pop[6:9,2:5]-real.pop[6:9,2:5]
ub.ca <- ub.pop[6:9,2:5]-real.pop[6:9,2:5]

out <- data.frame(yr=sc.pop[6:9,1], bb=NA, cck=NA, tp=NA, ys=NA)
for (i in 1:nrow(out)) {
  out[i,-1] <- paste0(round(ca[i,],2)," (",round(lb.ca[i,],2)," – ",round(ub.ca[i,],2),")")
}

View(cbind(out.agg, out[,-1]))

### ---- aggregate IEs 2019-2022----
a <- 100*(sum(sc.yr[6:9,2])-sum(real.yr[6:9,2]))/sum(sc.yr[6:9,2])
b <- 100*(sum(lb.t[6:9])-sum(real.yr[6:9,2]))/sum(lb.t[6:9])
c <- 100*(sum(ub.t[6:9])-sum(real.yr[6:9,2]))/sum(ub.t[6:9])

paste0(round(a,2)," (",round(b,2)," – ",round(c,2),")")
paste0(round(a,2),"% (95% CI: ",round(b,2),"% – ",round(c,2),"%)")

lb <- bb.eff[6:9,1:5]; ub <- bb.eff[6:9,c(1,6:9)]

for (i in 2:5) {
  print(colnames(sc2)[i])
  pt <- 100*(sum(sc2[6:9,i]) - sum(real2[6:9,i]) )/sum(sc2[6:9,i])
  lwr <- 100*(sum(lb[,i])-sum(real2[6:9,i]) )/sum(lb[,i])
  upr <- 100*(sum(ub[,i])-sum(real2[6:9,i]) )/sum(ub[,i])
  print(paste0(round(pt,2), " (", round(lwr,2), " - ", round(upr,2), ")") )
  
}

### ---- population size ----
pop <- read.csv('updated-data/population.csv')
sum(pop$pop[pop$PA %in% st])
sum(pop$pop[pop$PA %in% co])

### ---- summary table ----
listData <- loadRData('updated-data/list-dengue.rdata')
subset <- listData[c(1,10:35)]
subset[1] <- lapply(subset[1],function(x) normalize.pop(x,pop))
library(stringi)
# change here
ii <- which(rownames(listData$total)<='2018W52')
type=co

stat.fn <- function(var, type, ii){
  if(stri_detect_fixed(var,'max' ) ){
    value <- max(as.matrix(subset[[var]][ii,type]))
  }
  else if(stri_detect_fixed(var,'min' ) ){
    
    value <- min(as.matrix(subset[[var]][ii,type]))
  }
  else if(var %in% c('length_D',"Condo_n" ,"Landed_n","HDB_RU") ){
    value <- sum(as.matrix(subset[[var]][i,type]))
  }
  else {
    value <- mean(as.matrix(subset[[var]][ii,type]))
  }
  return(value)
}

df <- data.frame(var=1:27, value=NA, sd=NA, paste=NA)
for (i in 1:length(subset)) {
  df$var[i] <- names(subset)[i]
  df$value[i] <- round(stat.fn(names(subset)[i], type, ii),2)
  df$sd[i] <- round(sd(as.matrix(subset[[i]][ii,type])),2)
  if(i==1){
    df$value[i] <- round(stat.fn(names(subset)[i], type, ii)*100000,2)
    df$sd[i] <- round(sd(as.matrix(subset[[i]][ii,type]))*100000,2)
  }
  df$paste[i] <- paste0(df$value[i]," (",df$sd[i],")")
}
View(df[,c(1,4)])

subset <- res[[1]]$data.synth
for (i in 2:4) {
  for (j in 1:length(subset)) {
    subset[[j]] <- cbind(subset[[j]], res[[i]]$data.synth[[j]])
  }
}
subset <- lapply(subset, function(x){colnames(x)<-names(res); x})


## for synthetic control
# change here
ii <- which(rownames(listData$total)<'2018W27')
type='Yishun'

df <- data.frame(var=1:27, value=NA)
for (i in 1:length(subset)) {
  df$var[i] <- names(subset)[i]
  df$value[i] <- round(stat.fn(names(subset)[i], type, ii),2)
  df$sd[i] <- round(sd(as.matrix(subset[[i]][ii,type])),2)
  if(i==1){
    df$value[i] <- round(stat.fn(names(subset)[i], type, ii)*100000,2)
    df$sd[i] <- round(sd(as.matrix(subset[[i]][ii,type]))*100000,2)
  }
  df$paste[i] <- paste0(df$value[i]," (",df$sd[i],")")
}
View(df[,c(1,4)])

#### ----- Weights -----

subgrp = 'total'

res <- loadRData(paste0('model-out/m3-',subgrp,'.rdata') )

df <- data.frame(donor=rownames(res$`Bukit Batok`$v), bb.w=NA, cck.w= NA, tp.w=NA, ys.w=NA )
for (i in 1:4) {
  df[,i+1] <- res[[i]]$v[,1]
}

View(df)


