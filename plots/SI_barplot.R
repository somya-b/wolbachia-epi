#load libraries and data
library(dplyr)
library(stringi)
library(MSCMT)
library(grid)
source('../funcs.R')
source('SI_barplot F.R')

d1 <- read.csv('../updated-data/dates.csv')
d1$t <- c(2020.23, 2020.20, 2018.39, 2018.27)

#colors
col.rd = adjustcolor('#00468b',alpha.f = 0.4) 
col.sc = '#b30437'
col.real = 'white'
col.head = 'black'


## change here -
grp = "total"
model = 3

res <- loadRData(paste0('../model-out/m',model,'-',grp,'.rdata'))
res <- lapply(res, function(x){x$combined$subgrp})

# summarise data as to be plotted
sum.list <- c()
for (i in 1:4) {
  n <- as.integer((d1$t[i]%%1)*100)
  if (i==1 | i==2){
    yrs <- c(rep(2014,53),rep(2015,52),rep(2016,52),rep(2017,52),rep(2018,52),rep(2019,52),rep(2020,n-1),rep(d1$t[i],53-n+1),rep(2021,52),rep(2022,26))
  } else {
    yrs <- c(rep(2014,53),rep(2015,52),rep(2016,52),rep(2017,52),rep(2018,n-1),rep(d1$t[i],52-n+1),rep(2019,52),rep(2020,53),rep(2021,52),rep(2022,26))
  }
  aa <- data.frame(yrs= yrs,sc=res[[i]][,2]*100000,real=res[[i]][,1]*100000 ) 
  sum.model <- aa %>% group_by(yrs) %>% summarise( sc.sum = sum(sc), real.sum=sum(real)) 
  sum.list <- c(sum.list,list(ss=sum.model))
}
names(sum.list) <- d1$Area



cm=1/2.54

pdf(paste0('m',model,'/',grp,'.pdf'),family='Helvetica', height = 15*cm, width=15*cm, pointsize=12)
pushViewport(plotViewport(c(0.5,0.5,0.5,0.5)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))
xlm=c(0,15)
paneller(1,1)
paneller(2,3)
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
max.y <- max(sum.list[[1]][,2:3])
ylm <- c(0,max.y*1.2)*1000  
pushViewport(plotViewport(c(2,3,0,0), xscale=xlm, yscale=ylm))
panel.legend()
popViewport()
popViewport()
grid.text('Dengue Incidence (per 100,000 person in a year)',x = unit(0, "lines"), rot=90)
popViewport()
popViewport()
dev.off() 











