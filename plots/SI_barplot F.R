library(stringi)

xaxis.label <- function(town){
  lbl <- c("'14","'15","'16","'17","'18","'19","'20","'21","'22")
  t <- d1$t[town]
  tr<-sum.list[[town]]
  select <- which(tr$yrs==t)
  lbl <- append(lbl,paste0('Wb'),after = select-1)
  ss<-seq(0.5,by=1.5,length.out=10)
  for (i in 1:10) {
    grid.text(lbl[i],y=unit(-1.1,'lines'), x=unit(ss[i],'native'),gp=gpar(cex=0.8))
  }
}

yaxis.label <- function(town,vp){
  lbl <- grid.pretty(vp)
  for (i in 1:length(lbl)) {
    grid.text(lbl[i],y=unit(lbl[i],'native'), x=unit(-1.3,'lines'))
  }
}


draw.bar <- function(town,ylm){
  tr<-sum.list[[town]]
  pushViewport(plotViewport(c(0.1,0.1,0.1,0.1),yscale=ylm,xscale=c(0,15)))
  t <- d1$t[town]
  select <- which(tr$yrs==t)
  ss<-seq(0.5,by=1.5,length.out=10)
  grid.polygon(c(ss[select]-.2,ss[select]-.2,16,16),c(-1,ylm[2]+10,ylm[2]+10,-1),default.units = 'native',
               gp=gpar(fill=col.rd ,col=col.rd)) 
  for(i in 1:10){
    grid.polygon(c(ss[i]-.2,ss[i]-.2,ss[i]+.2,ss[i]+.2),c(0,tr$real.sum[i],tr$real.sum[i],0),
                 default.units = 'native', gp=gpar(fill=col.real,col='black'))
    grid.polygon(c(ss[i]+.2,ss[i]+.2,ss[i]+.6,ss[i]+.6),c(0,tr$sc.sum[i],tr$sc.sum[i],0),
                 default.units = 'native', gp=gpar(fill=col.sc,col='black'))
  }
  txt.town <- stri_trans_totitle(d1$Area[town])
  grid.text(paste0('(',letters[town],') ',txt.town),x=unit(0.02,'npc'),y=unit(0.97,'npc'),gp=gpar(col=col.head), 
            just = c('left','top'))
  popViewport()
}



paneller <- function(col.pos, town) {
  pushViewport(viewport(layout.pos.col=col.pos,layout.pos.row=1))
  tr <- sum.list[[town]]
  max.y <- max(tr[,2:3])  #---
  ylm <- c(0,max.y*1.2)  #---
  pushViewport(plotViewport(c(2.5,3,0,0), xscale=xlm, yscale=ylm, clip=T))
  draw.bar(town, ylm)
  popViewport()
  pushViewport(plotViewport(c(2.5,3,0,0), xscale=xlm, yscale=ylm))
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  ss<-seq(0.5,by=1.5,length.out=10)
  grid.xaxis(at=c(ss+.2),label=rep("",10))
  xaxis.label(town)
  vp<-current.viewport()$yscale
  grid.yaxis(label=F)
  yaxis.label(town,vp)
  popViewport()
  popViewport()
  pushViewport(viewport(layout.pos.col=col.pos,layout.pos.row=2))
  tr <- sum.list[[town+1]]
  max.y <- max(tr[,2:3])  #---
  ylm <- c(0,max.y*1.2)  #---
  pushViewport(plotViewport(c(3,3,0,0), xscale=xlm, yscale=ylm, clip=T))
  draw.bar(town+1, ylm)
  popViewport()
  pushViewport(plotViewport(c(3,3,0,0), xscale=xlm, yscale=ylm))
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  ss<-seq(0.5,by=1.5,length.out=10)
  grid.xaxis(at=c(ss+.2),label=rep("",10))
  xaxis.label(town+1)
  vp<-current.viewport()$yscale
  grid.yaxis(label=F)
  yaxis.label(town+1,vp)
  grid.text('Year',y=unit(-2,'lines'))
  popViewport()
  popViewport()
}

panel.legend <- function() {
  pushViewport(plotViewport(c(4,0.5,1.5,4), xscale=c(1,10), yscale=c(1,10)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2,widths = c(1,9))))
  pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
  pushViewport(plotViewport(c(0,0,1,0),xscale=c(1,10),yscale=c(1,10)))
  grid.polygon(c(1,1,9,9),c(10.5,11.5,11.5,10.5), default.units = 'native',gp=gpar(fill=col.real,col='black')) # for real
  grid.polygon(c(1,1,9,9),c(9,10,10,9),gp=gpar(fill=col.sc),default.units = 'native')
  grid.polygon(c(1,1,9,9),c(7.5,8.5,8.5,7.5),gp=gpar(fill=col.rd ,col=col.rd),default.units = 'native')
  popViewport()
  popViewport()
  pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
  pushViewport(plotViewport(c(0,0,1,0),xscale=c(1,10),yscale=c(1,10)))
  grid.text("Empirical",3.8,11, gp=gpar(cex=0.9, col='black'), default.units = 'native')
  grid.text("Synthetic Control",5.8,9.5,gp=gpar(cex=0.9, col=col.sc),default.units = 'native')
  grid.text("Release", 3.5, 8 ,gp=gpar(cex=0.9, col='#00377A'),default.units = 'native')
  popViewport()
  popViewport()
  popViewport()
  popViewport()
}


