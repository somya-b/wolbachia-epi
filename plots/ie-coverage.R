library(grid)
source('ie-coverage-funcs.R')

df <- read.csv('cov-ie.csv') # file containing yearly IEs and corresponding coverage at end of year
df$Coverage <- df$Coverage*100

shp=c(15,16,17,18)
blue=c('#0052A3','#70D7FF')
red=c('#AD1F23','#D06B62')
green=c('#216E4A','#9cc5a1')
purple=c('#7A40B5','#DDC8F9')
tots <- c('black','grey')
clrs <- list(tots=tots, blues=blue, reds=red,greens=green,purples=purple)
areas <- c('Aggregate',"Bukit Batok", "Choa Chu Kang", 'Tampines', 'Yishun')


cm=1/2.54
pdf('ie-vs-cov.pdf',height = 15*cm, width = 25*cm, family = 'Helvetica')
pushViewport(plotViewport(c(1,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2,widths=c(6,4))))
xlm=c(0,100)
ylm=c(0,100)
pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
pushViewport(plotViewport(c(5,1,5,1), xscale=xlm,yscale=ylm))
i=1
grid.text(paste0('(',letters[i+4],') ',areas[i]),x=0.02, y=0.96, gp=gpar(cex=0.9, col=clrs[[i]][1]),just = 'left', default.units = 'npc')
draw.lines(df[df$Town=='tot',],clrs[[i]])
grid.yaxis()
grid.xaxis()
panel.legend()
grid.text("Coverage (%)",y = unit(-3.2, "lines"),just = 'center')
popViewport()
popViewport()


pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2) ))

pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
pushViewport(plotViewport(c(1,3,2,0), xscale=xlm,yscale=ylm))
i=2
grid.yaxis()
grid.text(paste0('(',letters[i-1],') ',areas[i]),x=0.02, y=0.94, gp=gpar(cex=0.9, col=clrs[[i]][1]),just = 'left', default.units = 'npc')
draw.lines(df[df$Town=='bb',],clrs[[i]])
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
pushViewport(plotViewport(c(3,3,0,0), xscale=xlm,yscale=ylm))
grid.yaxis()
grid.xaxis()
i=3
grid.text(paste0('(',letters[i-1],') ',areas[i]),x=0.02, y=0.94, gp=gpar(cex=0.9, col=clrs[[i]][1]),just = 'left', default.units = 'npc')
draw.lines(df[df$Town=='cck',],clrs[[i]])
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
pushViewport(plotViewport(c(1,1,2,2), xscale=xlm,yscale=ylm))
i=4
grid.text(paste0('(',letters[i-1],') ',areas[i]),x=0.02, y=0.94, gp=gpar(cex=0.9, col=clrs[[i]][1]),just = 'left', default.units = 'npc')
draw.lines(df[df$Town=='tp',],clrs[[i]])
popViewport()
popViewport()

pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
pushViewport(plotViewport(c(3,1,0,2), xscale=xlm,yscale=ylm))
grid.xaxis()
i=5
grid.text(paste0('(',letters[i-1],') ',areas[i]),x=0.02, y=0.94, gp=gpar(cex=0.9, col=clrs[[i]][1]),just = 'left', default.units = 'npc')
draw.lines(df[df$Town=='ys',],clrs[[i]])
popViewport()
popViewport()
grid.text("Intervention Efficacy (%)",x = unit(0, "lines"), rot=90,just = 'center')
grid.text("Coverage (%)",y = unit(0, "lines"),just = 'center')
popViewport()
popViewport()
dev.off()
