
#########################
## plot time-series of daily mortality
##Figure 2
###################
par(mfrow=c(2,1))
plot(chic75s$death, xlab="Calendar Day", ylab="Daily Mortality",
     xaxt="n",cex=0.5,frame.plot=F,ylim=c(0,250), type = "l")
axis(1,at=c(1,182, 366,548, 732),lab=c("Jan 95","Jul 95","Jan 96","Jul 96", "Jan 97"),cex=0.5)
plot(chic75s$pm10tmean, xlab="Calendar Day", ylab="Daily PM10",
     xaxt="n",cex=0.5,frame.plot=F, type = "l")
axis(1,at=c(1,182, 366,548, 732),lab=c("Jan 95","Jul 95","Jan 96","Jul 96", "Jan 97"),cex=0.5)

## dev.off()