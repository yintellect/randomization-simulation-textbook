
cbind(numnear,numnearB,Y00,Y01,Y10,Y11)

distmattreat <- distmat + (1-treat)*100
numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treat

cbind(treat,cond,Y)

coords <- coords*1000
radius <- radius*1000
coordsNon <- coordsNon*1000

plot(coords,type="n",asp=1,axes=TRUE,xlab="",ylab="",xlim=c(-4000,4000),ylim=c(-4000,4000),xaxs="i")
symbols(coords,circles=rep(radius,N),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords,cex=1.5)

plot(coords,type="n",asp=1,axes=TRUE,xlab="",ylab="",xlim=c(-4000,4000),ylim=c(-4000,4000),xaxs="i")
symbols(coords[treat==1,],circles=rep(radius,ntreated),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords[cond==00,],cex=1.5)
points(coords[cond==10,],pch=19,col="grey",cex=1.5)
points(coords[cond==10,],cex=1.5)
points(coords[treat==1,],pch=19,cex=1.5)

plot(coords,type="n",asp=1,axes=TRUE,xlab="",ylab="",xlim=c(-4000,4000),ylim=c(-4000,4000),xaxs="i")
symbols(coords[treat==1,],circles=rep(radius,ntreated),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords[cond==00,],cex=1.5)
points(coords[cond==10,],pch=19,col="grey",cex=1.5)
points(coords[cond==10,],cex=1.5)
points(coords[treat==1,],pch=19,cex=1.5)
points(coordsNon,pch=4,cex=1.5)