# load libraries and data
library(readxl)
library(stringi)
library(grid)
source('../updated-data/names.R')

d1<-read.csv('../updated-data/dates.csv')
coverage <- read.csv('coverage.csv')
coverage$coverage <- 100*coverage$coverage
d1$t <- c(2020.23, 2020.20, 2018.39, 2018.27)
df <- c()
for (ss in st) {
  d<-as.data.frame(read_xlsx('FAeAegypti.xlsx',sheet=ss,range='A1:B263')) #excel file contains GAI per town
  df <- c(df,list(d=d))
}
names(df)<-st
df<-df[order(names(df))]

# getting donor GAI
donor <- as.data.frame(read_xlsx('FAeAegypti.xlsx',sheet=5))
fl <- which(stri_detect_fixed(colnames(donor), "FL") )
donor <- donor[,c(1,fl)]
donor[,-1] <- apply(donor[,-1],2,as.numeric)
donor$GAI <- apply(donor[,-1],1,function(x)mean(x,na.rm=T))
df$donor <- donor[,c('EYEW','GAI')]
df <- lapply(df, function(x){x[x$EYEW<=2022.26,]}) 
eyew <- df$`Bukit Batok`$EYEW

leg.fn <- function(){
  lines(x=c(1,20), y=c(0.28,0.28))
  lines(x=c(1,20), y=c(0.26,0.26), col=col.cp)
  polygon(x=c(1,1,20,20), y=.01*c(23,25,25,23), col=col.rd, border = F)
  lines(x=c(1,20), y=c(0.22,0.22),col='red')
  text('GAI', x= 27, y=0.28, adj=c(0,0.5))
  text('Coverage', x= 27, y=0.26, adj=c(0,0.5), col=col.cp)
  text('Release', x= 27, y=0.24, adj=c(0,0.5))#,col='#ea5766')
  text('Donor GAI',x= 27, y=0.22, adj=c(0,0.5),col='red')
}

col.rd="#F8C7CB40"
col.cp ='#0072bc'


margins <- list(c(3,4,2,0.5), #1
                c(3,0.5,2,4), #2
                c(5,4,0,0.5), #3
                c(5,0.5,0,4))

xlbl <- which(df[[1]]$EYEW %in% c(seq(2018,2022,by=1)+.01) )

#dev.off() 

cm=1/2.54
pdf('GAI-plot.pdf',width = 15*cm, height = 15*cm, family='Helvetica')
par(mfrow=c(2,2))
for (i in 1:4) {
  par(mar=margins[[i]])
  x=1:236
  plot(x=NA, y=NA, type='l', xaxt='n',ylim=range(0,0.3), xlab = '',yaxt='n', ylab='',xlim=range(1,236))
  select <- which(df[[i]]$EYEW==d1$t[i])
  polygon(x=c(select,select,250,250),y=c(-2,0.5,0.5,-2), col = col.rd, border = F)
  lines(x=x, y=df[[i]]$GAI)
  lines(x=x, y=df$donor$GAI, col='red')
  axis(1,at=xlbl,labels = paste0("'",seq(18,22)), padj = -1.8, hadj = -0.2 )
  text(x=-5,y=0.3, paste0("(",letters[i],") ", d1$Area[i]), adj=c(0,0.5))
  if(i==1 | i==3) {axis(2,at=seq(0,0.3,by=0.1),labels =seq(0,0.3,by=0.1)  ,las=1, hadj=0.7 )}
  if(i==1){leg.fn()}
  par(new=T)
  newd <- coverage[coverage$town==d1$Area[i],]
  x<-which(df[[i]]$EYEW %in% newd$release)
  plot(x,newd$coverage,type='l', col=col.cp,xaxt='n', xlab = '',yaxt='n', ylab='',xlim=range(1,236),ylim=range(0,100))
  points(x,newd$coverage,pch=16, col=col.cp)
  if(i==2 | i==4) {axis(4,at=seq(0,100,by=25),las=1, hadj=0.2)}
  
  
}
mtext('Gravitrap Index', 2, outer=T, line = -1.5)
mtext('Time period (E-weeks)', 1, outer=T, line = -3)
mtext('Coverage (%)',4,outer=T, line=-1.5, col=col.cp)
dev.off()

