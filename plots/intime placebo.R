source('../funcs.R')

d1 <- read.csv('../updated-data/dates.csv')
mapT <- read.csv('../updated-data/mapT.csv')

margins <- list(c(3,4,1,0), #1
                c(3,3,1,1), #2
                c(4,4,0,0), #3
                c(4,3,0,1))

xlbl <- which(mapT$ey.ew %in% c(seq(2014,2020,by=1)+.01) )

leg.fn <- function(){
  
  l1=27.5; l2=l1-2; pt=l2-2; 
  p1=pt-1; p2=pt+1; 
  
  lines(x=c(1,25), y=c(l1,l1), col='#f18f99')
  lines(x=c(1,25), y=c(l2,l2))
  polygon(x=c(1,1,25,25), y=c(p1,p2,p2,p1), col=col.rd, border = F)
  text('Synthetic Control', x= 32, y=l1, col='#ea5766', adj=c(0,0.5))
  text('Observed', x= 32, y=l2, adj=c(0,0.5))
  text('Pseudo-release', x= 32, y=pt, adj=c(0,0.5))
}

col.rd = "#F8C7CB40"  
col.sc = "#EA576640"

cm=1/2.54
### --- change here -----
cv <- loadRData('../model-out/cv_M3.rdata')


pdf('intime-m3.pdf', height = 15*cm, width=15*cm, family='Helvetica')

par(mfrow=c(2,2))
for (town in 1:4) {
  par(mar=margins[[town]])
  if(town<3){x2=335}
  if(town>2){x2=247}
  plot(x=NA,y=NA, ylim=range(0,30), xlim=range(1,x2),xaxt='n',ylab = NA, xlab = NA, las=1)
  end = which(mapT$time == d1$Date[town])-1
  start = end - 52 +1
  polygon(x=c(start,start,350,350), y=c(-1,31,31,-1), col = col.rd, border = F)
  obs <- c(cv[[2]][[town]]$data.treat$subgrp[1:end])*100000
  lines(x=1:end, y=obs)
  for (j in 2:53) {
    sc <- c(cv[[j]][[town]]$data.synth$subgrp[1:end])*100000
    lines(x=1:end, y=sc,col=col.sc)
  }

  axis(1,at=xlbl, labels = seq(2014,2020),gap.axis = 1/4, padj = 0.4,hadj = 0.8,las=2)
  text(paste0("(",letters[town],") ",d1$Area[town] ), x=-2.5, y=29.5, adj=c(0,0.5))
  if(town==1){leg.fn()}
}
mtext('Dengue incidence rate per 100,000 persons', 2, outer=T, line = -1.5)
mtext('Time period (E-weeks)', 1, outer=T, line = -1.1)
dev.off()


