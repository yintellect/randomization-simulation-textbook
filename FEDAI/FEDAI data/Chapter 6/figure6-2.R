quartz(width=7,height=5.25,title="2SLS Confidence Intervals")

par(family="Gill Sans MT",font.main=1)


civic2008 <- c(-.4768307,.2607122)*100
hawthorne2008 <- c(-.1160171,.3756754)*100
self2008 <- c(.0627826,.3245638)*100
neigh2008 <- c(.0687942,.2245515)*100
pooled2008 <- c(.0857956,.2280007)*100

civic2010 <- c( -.292202,.4310046)*100
hawthorne2010 <- c(-.1119799,.3919152)*100
self2010 <- c( .0260919,.295197)*100
neigh2010 <- c( .065825,.2261042)*100
pooled2010 <- c( .0759035,.2223587)*100

civic2008b <-  -.1080592*100
hawthorne2008b <-  .1298291*100
self2008b <-  .1936732*100
neigh2008b <- .1466729*100
pooled2008b <- 0.1568981*100

civic2010b <- .0694013*100
hawthorne2010b <-  .1399677*100
self2010b <- .1606444*100
neigh2010b <- .1459646*100
pooled2010b <- .1491311*100


plot(0,0,type="n",xlim=c(-100,100),ylim=c(-1,21),ylab="",xlab="Estimated CACE 95% Confidence Interval",yaxt="n")

lines(lwd=1.5,x=civic2008,y=c(20,20))
lines(lwd=1.5,x=c(civic2008[1],civic2008[1]),y=c(19.75,20.25))
lines(lwd=1.5,x=c(civic2008[2],civic2008[2]),y=c(19.75,20.25))
lines(lwd=1.5,x=c(civic2008b,civic2008b),y=c(19.75,20.25))
text(sprintf("%.1f",civic2008b),x=civic2008b,y=20,pos=3)

lines(lwd=1.5,x=hawthorne2008,y=c(18,18))
lines(lwd=1.5,x=c(hawthorne2008[1],hawthorne2008[1]),y=c(17.75,18.25))
lines(lwd=1.5,x=c(hawthorne2008[2],hawthorne2008[2]),y=c(17.75,18.25))
lines(lwd=1.5,x=c(hawthorne2008b,hawthorne2008b),y=c(17.75,18.25))
text(sprintf("%.1f",hawthorne2008b),x=hawthorne2008b,y=18,pos=3)

lines(lwd=1.5,x=self2008,y=c(16,16))
lines(lwd=1.5,x=c(self2008[1],self2008[1]),y=c(15.75,16.25))
lines(lwd=1.5,x=c(self2008[2],self2008[2]),y=c(15.75,16.25))
lines(lwd=1.5,x=c(self2008b,self2008b),y=c(15.75,16.25))
text(sprintf("%.1f",self2008b),x=self2008b,y=16,pos=3)

lines(lwd=1.5,x=neigh2008,y=c(14,14))
lines(lwd=1.5,x=c(neigh2008[1],neigh2008[1]),y=c(13.75,14.25))
lines(lwd=1.5,x=c(neigh2008[2],neigh2008[2]),y=c(13.75,14.25))
lines(lwd=1.5,x=c(neigh2008b,neigh2008b),y=c(13.75,14.25))
text(sprintf("%.1f",neigh2008b),x=neigh2008b,y=14,pos=3)

lines(lwd=1.5,x=pooled2008,y=c(12,12))
lines(lwd=1.5,x=c(pooled2008[1],pooled2008[1]),y=c(11.75,12.25))
lines(lwd=1.5,x=c(pooled2008[2],pooled2008[2]),y=c(11.75,12.25))
lines(lwd=1.5,x=c(pooled2008b,pooled2008b),y=c(11.75,12.25))
text(sprintf("%.1f",pooled2008b),x=pooled2008b,y=12,pos=3)

lines(x = c(-1000,1090),y=c(10,10))

lines(lwd=1.5,x=civic2010,y=c(8,8))
lines(lwd=1.5,x=c(civic2010[1],civic2010[1]),y=c(7.75,8.25))
lines(lwd=1.5,x=c(civic2010[2],civic2010[2]),y=c(7.75,8.25))
lines(lwd=1.5,x=c(civic2010b,civic2010b),y=c(7.75,8.25))
text(sprintf("%.1f",civic2010b),x=civic2010b,y=8,pos=3)

lines(lwd=1.5,x=hawthorne2010,y=c(6,6))
lines(lwd=1.5,x=c(hawthorne2010[1],hawthorne2010[1]),y=c(5.75,6.25))
lines(lwd=1.5,x=c(hawthorne2010[2],hawthorne2010[2]),y=c(5.75,6.25))
lines(lwd=1.5,x=c(hawthorne2010b,hawthorne2010b),y=c(5.75,6.25))
text(sprintf("%.1f",hawthorne2010b),x=hawthorne2010b,y=6,pos=3)

lines(lwd=1.5,x=self2010,y=c(4,4))
lines(lwd=1.5,x=c(self2010[1],self2010[1]),y=c(3.75,4.25))
lines(lwd=1.5,x=c(self2010[2],self2010[2]),y=c(3.75,4.25))
lines(lwd=1.5,x=c(self2010b,self2010b),y=c(3.75,4.25))
text(sprintf("%.1f",self2010b),x=self2010b,y=4,pos=3)

lines(lwd=1.5,x=neigh2010,y=c(2,2))
lines(lwd=1.5,x=c(neigh2010[1],neigh2010[1]),y=c(1.75,2.25))
lines(lwd=1.5,x=c(neigh2010[2],neigh2010[2]),y=c(1.75,2.25))
lines(lwd=1.5,x=c(neigh2010b,neigh2010b),y=c(1.75,2.25))
text(sprintf("%.1f",neigh2010b),x=neigh2010b,y=2,pos=3)

lines(lwd=1.5,x=pooled2010,y=c(0,0))
lines(lwd=1.5,x=c(pooled2010[1],pooled2010[1]),y=c(-0.25,0.25))
lines(lwd=1.5,x=c(pooled2010[2],pooled2010[2]),y=c(-0.25,0.25))
lines(lwd=1.5,x=c(pooled2010b,pooled2010b),y=c(-0.25,0.25))
text(sprintf("%.1f",pooled2010b),x=pooled2010b,y=0,pos=3)

text(x=105,"Civic Duty 2008",y=c(20,20),pos=2)
text(x=105,"Hawthorne 2008",y=c(18,18),pos=2)
text(x=105,"Self 2008",y=c(16,16),pos=2)
text(x=105,"Neighbors 2008",y=c(14,14),pos=2)
text(x=105,"Pooled 2008",y=c(12,12),pos=2)

text(x=-105,"Hansen J = 2.948",y=c(12.75),pos=4)
text(x=-105,expression(paste(italic("p")," = 0.400")),y=c(11),pos=4)


text(x=105,"Civic Duty 2010",y=c(8,8),pos=2)
text(x=105,"Hawthorne 2010",y=c(6,6),pos=2)
text(x=105,"Self 2010",y=c(4,4),pos=2)
text(x=105,"Neighbors 2010",y=c(2,2),pos=2)
text(x=105,"Pooled 2010",y=c(0,0),pos=2)

text(x=-105,"Hansen J = 0.253",y=c(0.75),pos=4)
text(x=-105,expression(paste(italic("p")," = 0.969")),y=c(-1),pos=4)

