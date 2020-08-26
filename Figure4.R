pdf("Figure4.pdf", paper="special", family = "Helvetica", onefile = FALSE, width= 6.83, height= 3.27, colormodel="rgb",  pointsize=12)

op <- par(mar = c(0.5, 0.25, 0.5, 0), oma=c(3,5.4,1.2,8), mfcol=c(3,5), pty="s", las=1, ps=12)


color=c(rgb(204,204,204, alpha= 100, maxColorValue = 255), rgb(255,255,165, alpha= 100, maxColorValue = 255) )
h=22 #gamma = 5%

inv<- read.csv("Results/Scen1_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen1_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])
propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n')

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")

points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")

stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=1000, y=120, "NC1", cex=1, font=2)
text(x=1000, y=108, expression(paste(italic('K'), " = 50, ", italic('Km'), " = 10", " | ", italic('K'), " = 50, ", italic('Km'), " = 10")), cex=0.5, font=2)

text(x=-950, y=50, "Whole area", cex=1, font=2, srt=90)
par(xpd=F)
##########################################################
inv<- read.csv("Results/Scen2_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen2_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[13+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[13+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n')

x<-cbind(temp[2:6],rev(temp[2:6]))
yI<- cbind(propISourceTemp[2:6],rev(propICornerTemp[2:6]) )
yL<- cbind(propLSourceTemp[2:6],rev(propLCornerTemp[2:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[2:6], propLCornerTemp[2:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[2:6], propLCornerTemp[2:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")
points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=-950, y=50, "Restricted area", cex=1, font=2, srt=90)
par(xpd=F)


##########################################################
inv<- read.csv("Results/Scen3_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen3_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n')
axis(1, at=c(0,1000, 2000), labels=c(0,1000, 2000), cex.axis=0.8)

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp[4:6], propLSourceTemp[4:6], lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp[4:6], propLSourceTemp[4:6], pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp[4:6], propISourceTemp[4:6], lty=2, lwd=1, col="goldenrod")
lines(temp[4:6], propICornerTemp[4:6], lty=2, lwd=1, col="goldenrod")
points(temp[4:6], propISourceTemp[4:6], pch=17, cex=1, col="goldenrod")
points(temp[4:6], propICornerTemp[4:6], pch=16, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1700, y0= stinv-5, x1 = 1700, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1900, y0= stloc+5, x1 = 1900, y1 = stloc-5, code=1, length = 0.05, lwd=1, col="gray50", angle = 20)


par(xpd=NA)
text(x=-950, y=50, "Two invasions", cex=1, font=2, srt=90)
par(xpd=F)


################################################
############## NC2 #############################
################################################
color=c(rgb(204,204,204, alpha= 100, maxColorValue = 255), rgb(255,255,165, alpha= 100, maxColorValue = 255) )
h=22 +24 #5%

inv<- read.csv("Results/Scen1_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen1_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])
propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[5:6],rev(temp[5:6]))
yI<- cbind(propISourceTemp[5:6],rev(propICornerTemp[5:6]) )
yL<- cbind(propLSourceTemp[5:6],rev(propLCornerTemp[5:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[5:6], propLCornerTemp[5:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[5:6], propLCornerTemp[5:6], pch=17, cex=1, col="gray50")

par(xpd=NA)
polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")

points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")

stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1750, y0= stinv-5, x1 = 1750, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1750, y0= stloc+5, x1 = 1750, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=1000, y=120, "NC2", cex=1, font=2)
text(x=1000, y=108, expression(paste(italic('K'), " = 500, ", italic('Km'), " = 10", " | ", italic('K'), " = 500, ", italic('Km'), " = 10")), cex=0.5, font=2)

par(xpd=F)

##########################################################
inv<- read.csv("Results/Scen2_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen2_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[13+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[13+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")
points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)


##########################################################
inv<- read.csv("Results/Scen3_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen3_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", yaxt='n', xaxt='n')
axis(1, at=c(0,1000, 2000), labels=c(0,1000, 2000), cex.axis=0.8)

x<-cbind(temp[5:6],rev(temp[5:6]))
yI<- cbind(propISourceTemp[5:6],rev(propICornerTemp[5:6]) )
yL<- cbind(propLSourceTemp[5:6],rev(propLCornerTemp[5:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp[5:6], propLSourceTemp[5:6], lty=2, lwd=1, col="gray50")
lines(temp[5:6], propLCornerTemp[5:6], lty=2, lwd=1, col="gray50")
points(250+temp[5], propLSourceTemp[5], pch=16, cex=1, col="gray50")
points(250+temp[5], propLCornerTemp[5], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp[5:6], propISourceTemp[5:6], lty=2, lwd=1, col="goldenrod")
lines(temp[5:6], propICornerTemp[5:6], lty=2, lwd=1, col="goldenrod")
points(temp[5:6], propISourceTemp[5:6], pch=17, cex=1, col="goldenrod")
points(temp[5:6], propICornerTemp[5:6], pch=16, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1700, y0= stinv+5, x1 = 1700, y1 = stinv -5, code=1, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1900, y0= stloc+5, x1 = 1900, y1 = stloc-5, code=1, length = 0.05, lwd=1, col="gray50", angle = 20)


################################################
############## NC3 #############################
################################################
color=c(rgb(204,204,204, alpha= 100, maxColorValue = 255), rgb(255,255,165, alpha= 100, maxColorValue = 255) )
h=22 +24*2 #5%

inv<- read.csv("Results/Scen1_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen1_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])
propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")

par(xpd=NA)
polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")

points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")

stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1750, y0= stinv-5, x1 = 1750, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1750, y0= stloc+5, x1 = 1750, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=1000, y=120, "NC3", cex=1, font=2)
text(x=1000, y=108, expression(paste(italic('K'), " = 500, ", italic('Km'), " = 100", " | ", italic('K'), " = 500, ", italic('Km'), " = 100")), cex=0.5, font=2)

par(xpd=F)

##########################################################
inv<- read.csv("Results/Scen2_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen2_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[13+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[13+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[2:6],rev(temp[2:6]))
yI<- cbind(propISourceTemp[2:6],rev(propICornerTemp[2:6]) )
yL<- cbind(propLSourceTemp[2:6],rev(propLCornerTemp[2:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[2:6], propLCornerTemp[2:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[2:6], propLCornerTemp[2:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")
points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)


##########################################################
inv<- read.csv("Results/Scen3_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen3_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", yaxt='n', xaxt='n')
axis(1, at=c(0,1000, 2000), labels=c(0,1000, 2000), cex.axis=0.8)

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp[4:6], propLSourceTemp[4:6], lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(250+temp[4:5], propLSourceTemp[4:5], pch=16, cex=1, col="gray50")
points(250+temp[4:5], propLCornerTemp[4:5], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp[4:6], propISourceTemp[4:6], lty=2, lwd=1, col="goldenrod")
lines(temp[4:6], propICornerTemp[4:6], lty=2, lwd=1, col="goldenrod")
points(temp[4:6], propISourceTemp[4:6], pch=17, cex=1, col="goldenrod")
points(temp[4:6], propICornerTemp[4:6], pch=16, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1700, y0= stinv+5, x1 = 1700, y1 = stinv -5, code=1, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1900, y0= stloc+5, x1 = 1900, y1 = stloc-5, code=1, length = 0.05, lwd=1, col="gray50", angle = 20)


################################################
############## NC4 #############################
################################################
color=c(rgb(204,204,204, alpha= 100, maxColorValue = 255), rgb(255,255,165, alpha= 100, maxColorValue = 255) )
h=22 +24*3 #5%

inv<- read.csv("Results/Scen1_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen1_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])
propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")

par(xpd=NA)
polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")

points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")

stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1750, y0= stinv-5+10, x1 = 1750, y1 = stinv +5+10, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1750, y0= stloc+5-10, x1 = 1750, y1 = stloc-5-10, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=1000, y=120, "NC4", cex=1, font=2)
text(x=1000, y=108, expression(paste(italic('K'), " = 50, ", italic('Km'), " = 10", " | ", italic('K'), " = 500, ", italic('Km'), " = 100")), cex=0.5, font=2)

par(xpd=F)

##########################################################
inv<- read.csv("Results/Scen2_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen2_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[13+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[13+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[2:6],rev(temp[2:6]))
yI<- cbind(propISourceTemp[2:6],rev(propICornerTemp[2:6]) )
yL<- cbind(propLSourceTemp[2:6],rev(propLCornerTemp[2:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[2:6], propLCornerTemp[2:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[2:6], propLCornerTemp[2:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")
points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

##########################################################
inv<- read.csv("Results/Scen3_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen3_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", yaxt='n', xaxt='n')
axis(1, at=c(0,1000, 2000), labels=c(0,1000, 2000), cex.axis=0.8)

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp[4:6], propLSourceTemp[4:6], lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp[4:6], propLSourceTemp[4:6], pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp[4:6], propISourceTemp[4:6], lty=2, lwd=1, col="goldenrod")
lines(temp[4:6], propICornerTemp[4:6], lty=2, lwd=1, col="goldenrod")
points(temp[4:6], propISourceTemp[4:6], pch=17, cex=1, col="goldenrod")
points(temp[4:6], propICornerTemp[4:6], pch=16, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1700, y0= stinv+5, x1 = 1700, y1 = stinv -5, code=1, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1900, y0= stloc+5, x1 = 1900, y1 = stloc-5, code=1, length = 0.05, lwd=1, col="gray50", angle = 20)


################################################
############## NC5 #############################
################################################
color=c(rgb(204,204,204, alpha= 100, maxColorValue = 255), rgb(255,255,165, alpha= 100, maxColorValue = 255) )
h=22 +24*4 #5%

inv<- read.csv("Results/Scen1_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen1_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])
propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")

par(xpd=NA)
polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")

points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")

stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1750, y0= stinv-5-2.5, x1 = 1750, y1 = stinv +5-2.5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1750, y0= stloc+5+2.5, x1 = 1750, y1 = stloc-5+ 2.5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=1000, y=120, "NC5", cex=1, font=2)
text(x=1000, y=108, expression(paste(italic('K'), " = 500, ", italic('Km'), " = 100", " | ", italic('K'), " = 50, ", italic('Km'), " = 10")), cex=0.5, font=2)


par(xpd=NA)
d=750
f=20
e=7
rect(d+1500,95-f+e,d+2000,120-f, col=color[1], border="black")
text(d+2000, (95-f+e+120-f)/2, "Local", cex=0.8, pos=4)

rect(d+1500,69-f+e,d+2000,94-f, col=color[2], border="black")
text(d+2000, (69-f+e+94-f)/2, "Invasive", cex=0.8, pos=4)

temp <- legend(1425+750, 55-f+10+e*2, c(" "," "), lty=rep(2,2), col=rep(1,7),  pch=c(17,16), bty="n", lwd=1, pt.bg="white", cex=1, y.intersp=1.25)

text(temp$rect$left + temp$rect$w-475, temp$text$y, c("Source","Corner"), pos = 4, cex=0.8)

par(xpd=F)


##########################################################
inv<- read.csv("Results/Scen2_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen2_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[13+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[13+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", xaxt='n', yaxt='n')

x<-cbind(temp[2:6],rev(temp[2:6]))
yI<- cbind(propISourceTemp[2:6],rev(propICornerTemp[2:6]) )
yL<- cbind(propLSourceTemp[2:6],rev(propLCornerTemp[2:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp, propLSourceTemp, lty=2, lwd=1, col="gray50")
lines(temp[2:6], propLCornerTemp[2:6], lty=2, lwd=1, col="gray50")
points(temp, propLSourceTemp, pch=16, cex=1, col="gray50")
points(temp[2:6], propLCornerTemp[2:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp, propISourceTemp, lty=2, lwd=1, col="goldenrod")
lines(temp, propICornerTemp, lty=2, lwd=1, col="goldenrod")
points(temp, propISourceTemp, pch=16, cex=1, col="goldenrod")
points(temp, propICornerTemp, pch=17, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1500, y0= stinv-5, x1 = 1500, y1 = stinv +5, code=2, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1500, y0= stloc+5, x1 = 1500, y1 = stloc-5, code=2, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
d=750
f=20
e=7
rect(d+1500,95-f+e,d+2000,120-f, col=color[1], border="black")
text(d+2000, (95-f+e+120-f)/2, "Local", cex=0.8, pos=4)

rect(d+1500,69-f+e,d+2000,94-f, col=color[2], border="black")
text(d+2000, (69-f+e+94-f)/2, "Invasive", cex=0.8, pos=4)

temp <- legend(1425+750, 55-f+10+e*2, c(" "," "), lty=rep(2,2), col=rep(1,7),  pch=c(17,16), bty="n", lwd=1, pt.bg="white", cex=1, y.intersp=1.25)

text(temp$rect$left + temp$rect$w-475, temp$text$y, c("Source","Corner"), pos = 4, cex=0.8)
par(xpd=F)

##########################################################
inv<- read.csv("Results/Scen3_NC_mean.csv", header=T)
loc<- read.csv("Results/Scen3_NCbis_mean.csv", header=T)

propISourceTemp <- 100*rev(1-inv[h, paste(coor[1+(0:5)*25,1])])
propLSourceTemp <- 100*rev(loc[h, paste(coor[1+(0:5)*25,1])])

propICornerTemp <- 100*rev(1-inv[h, paste(coor[25+(0:5)*25,1])])

propLCornerTemp <- 100*rev(loc[h, paste(coor[25+(0:5)*25,1])])

temp<-c(100,250,500,1000, 1500, 2000)

plot(temp, propISourceTemp, type="n", lty=1, lwd=3, bty="l", xlab=" ", ylab=" ", cex.axis=0.8, xaxs="i",yaxs="i", las=1, xlim=c(0, 2020), ylim=c(0, 100), col="darkred", yaxt='n', xaxt='n')
axis(1, at=c(0,1000, 2000), labels=c(0,1000, 2000), cex.axis=0.8)

x<-cbind(temp[4:6],rev(temp[4:6]))
yI<- cbind(propISourceTemp[4:6],rev(propICornerTemp[4:6]) )
yL<- cbind(propLSourceTemp[4:6],rev(propLCornerTemp[4:6]) )

polygon(x, yL, col=color[1], border=NA)
lines(temp[4:6], propLSourceTemp[4:6], lty=2, lwd=1, col="gray50")
lines(temp[4:6], propLCornerTemp[4:6], lty=2, lwd=1, col="gray50")
points(temp[4:6], propLSourceTemp[4:6], pch=16, cex=1, col="gray50")
points(temp[4:6], propLCornerTemp[4:6], pch=17, cex=1, col="gray50")


polygon(x, yI, col= color[2], border=NA)
lines(temp[4:6], propISourceTemp[4:6], lty=2, lwd=1, col="goldenrod")
lines(temp[4:6], propICornerTemp[4:6], lty=2, lwd=1, col="goldenrod")
points(temp[4:6], propISourceTemp[4:6], pch=17, cex=1, col="goldenrod")
points(temp[4:6], propICornerTemp[4:6], pch=16, cex=1, col="goldenrod")


stinv<-as.numeric((propICornerTemp[5]+propISourceTemp[5])/2)
stloc<-as.numeric((propLCornerTemp[5]+propLSourceTemp[5])/2)

arrows(x0=1700, y0= stinv+5, x1 = 1700, y1 = stinv -5, code=1, length = 0.05, lwd=1, col="goldenrod", angle = 20)
arrows(x0=1900, y0= stloc+5, x1 = 1900, y1 = stloc-5, code=1, length = 0.05, lwd=1, col="gray50", angle = 20)

par(xpd=NA)
text(x=-4200, y=-49, "Time (generations)", cex=1.5, font=2)
text(x=-12000, y=165, "Introgression (%)", cex=1.5, srt=90, font=2)

d=750
f=20
e=7
rect(d+1500,95-f+e,d+2000,120-f, col=color[1], border="black")
text(d+2000, (95-f+e+120-f)/2, "Invasive 2", cex=0.8, pos=4)

rect(d+1500,69-f+e,d+2000,94-f, col=color[2], border="black")
text(d+2000, (69-f+e+94-f)/2, "Invasive 1", cex=0.8, pos=4)

temp <- legend(1425+750, 55-f+10+e*2, c(" "," "), lty=rep(2,2), col=rep(1,7),  pch=c(17,16), bty="n", lwd=1, pt.bg="white", cex=1, y.intersp=1.25)

text(temp$rect$left + temp$rect$w-475, temp$text$y, c("Source","Corner"), pos = 4, cex=0.8)

par(op)
dev.off()

