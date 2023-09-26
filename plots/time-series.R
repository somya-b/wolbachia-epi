library(stringi)
source('../funcs.R')

d1 <- read.csv('../updated-data/dates.csv')
d1$t <- c(2020.23, 2020.20, 2018.39, 2018.27)

mapT <- read.csv('../updated-data/mapT.csv')
col.rd="#F8C7CB40"
col.sc = "#ed0000"

xlbl <- which(mapT$ey.ew %in% c(seq(2014,2022,by=1)+.01) )

leg.fn <- function(ymax){
  if(ymax==50){
    l1=45; l2=41; pt=37; p1=35; p2=39; 
  }
  if(ymax==30){
    l1=27; l2=24.5; pt=22; p1=21; p2=23
  }
  if(ymax==15){
    l1=13.6;l2=12.5; pt=11.4; p1=10.9; p2=11.9
  }
  lines(x=c(1,25), y=c(l1,l1), col=col.sc)
  lines(x=c(1,25), y=c(l2,l2))
  polygon(x=c(1,1,25,25), y=c(p1,p2,p2,p1), col=col.rd, border = F)
  text('Synthetic Control', x= 32, y=l1, col=col.sc, adj=c(0,0.5))
  text('Observed', x= 32, y=l2, adj=c(0,0.5))
  text('Release', x= 32, y=pt, adj=c(0,0.5),col='#ea5766')
}

margins <- list(c(3,4,1,0), #1
                c(3,3,1,1), #2
                c(4,4,0,0), #3
                c(4,3,0,1))

cm=1/2.54
#### --- change here ------
subgrp = 'age61'
ymax = 30

res <- loadRData(paste0('../model-out/m3-',subgrp,'.rdata') )

sc <- c(); real <- c()
for(i in 1:4){
  sc <- cbind(sc, res[[i]]$combined$subgrp[,2])
  real <- cbind(real, res[[i]]$combined$subgrp[,1]) 
}
colnames(sc)<- colnames(real)<-names(res)

sc <- as.data.frame(sc); real <- as.data.frame(real)
synth <- 100000*sc
obs <- 100000*real

#dev.off()

pdf(paste0('m3/ts-',subgrp,'.pdf'), width=15*cm, height = 15*cm, family = "Helvetica")
par(mfrow=c(2,2))
for (i in 1:4) {
  par(mar=margins[[i]])
  plot(x=NA,y=NA, type='l', ylim=range(0,ymax), xlim=range(1,444),xaxt='n',ylab = NA, xlab = NA, las=1)
  start = which(mapT$ey.ew == d1$t[i])
  polygon(x=c(start,start,550,550), y=c(-3,52,52,-3), col = col.rd, border = F)
  axis(1,at=xlbl, labels = seq(2014,2022),gap.axis = 1/4, padj = 0.4,hadj = 0.8,las=2)
  lines(x=mapT$sno,y=obs[,i])
  lines(x=mapT$sno, synth[,i], col=col.sc, lwd=1)
  if(ymax==50){ ytxt=49 }
  if(ymax==30){ytxt=29.5}
  if(ymax==15){ytxt=14.7}
  text(paste0("(",letters[i],") ",colnames(obs)[i] ), x=0, y=ytxt, adj=c(0,0.5))
  if (i==1) {leg.fn(ymax)}
}
mtext('Dengue incidence rate per 100,000 persons', 2, outer=T, line = -1.5)
mtext('Time period (E-weeks)', 1, outer=T, line = -1.1)
dev.off() 

