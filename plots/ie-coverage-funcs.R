draw.lines <- function(xy,clr){
  grid.rect(gp=gpar(fill=NA))
  grid.lines(x=c(xy$Coverage), y=c(xy$IE.lb), gp=gpar(col=clr[2],lty=2),default.units = 'native')
  grid.lines(x=c(xy$Coverage), y=c(xy$IE.ub), gp=gpar(col=clr[2],lty=2),default.units = 'native')
  grid.lines(x=c(xy$Coverage), y=c(xy$IE), gp=gpar(col=clr[1]),default.units = 'native')
  grid.points(x=xy$Coverage, y=xy$IE, pch=shp, gp=gpar(col=clr[1], cex=0.8))
}


panel.legend <- function(){
  x.pos <- 0.02
  y.pos1 <- c(0.97,0.93,0.89,0.85,0.81)
  y.pos2 <- c(0.16,0.12,0.08,0.04,0.0)
  yrs <- c(2019,2020,2021,2022)
  areas <- c('Aggregate',"Bukit Batok", "Choa Chu Kang", 'Tampines', 'Yishun')
  for (i in 1:5) {
    
    # grid.lines(x=c(x.pos,x.pos*1.5), y=rep(y.pos1[i],2), gp=gpar(col=clrs[i],cex=2) )
    # grid.text(areas[i], x=x.pos*2, y=y.pos1[i], gp=gpar(cex=0.9, col=clrs[i]),just = 'left', default.units = 'npc')
    if(i==5)break
    grid.points(x=0.85, y=y.pos2[i], pch=shp[i], gp=gpar(cex=0.75), default.units = 'npc')
    grid.text(yrs[i], x=0.88, y=y.pos2[i], gp=gpar(cex=0.9),just = 'left', default.units = 'npc')
  }
  # grid.lines(x=c(0.05,0.05),y=c(0.75,0.79))
  # grid.text('95% CI', x=0.15, y=0.77,gp=gpar(cex=0.9),just = 'left')
}
