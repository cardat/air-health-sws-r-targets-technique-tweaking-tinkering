##### epiweb.R ###########
##### Yun LU ###########
##### 09/07/2007  ######

##model-checking sample code for case-crossover analyses

## load survival library in order to fit natural spline
## for temperature and dew point temperature
library(survival) 
library(splines)
source('fcn.r')

##load data
##people age >=75 years old in Chicago
##date 19950101 - 19961231
load("epichic.rda")


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

dev.off()
##########################################

          
    ## Time-stratified case-crossover design (TSD)
##reference window: the same day of the week in the same month and year

### estimate beta using model.tsd.clr
####TSD using conditional logistic regression
        fit.tsd.clr=model.tsd.clr(chic75s)
         ##b.tsd.clr is the percentage increase of relative risk per 10microgram pm10 increase
         b.tsd.clr=fit.tsd.clr$coeff[1]*1000
         ##se.tsd.clr is the standard error of the estimate
         se.tsd.clr=sqrt(fit.tsd.clr$var[1])*1000 
         
      ##################################################################         
     ### estimate beta using model.tsd
     ##TSD using time series log-linear model using indicator variables for strata
##without over-dispersion
   ### the estimator and standard error should be the same as obtained using model.tsd.clr
   ### model.tsd allows for model-checking using log-linear model diagnostics
   #######################################################################
        fit.tsd=model.tsd(chic75s)
        b.tsd=fit.tsd$coeff[2]*1000
         se.tsd=summary(fit.tsd)$coeff[2,][[2]]*1000
    
    ##############################################################     
        ### estimate beta using model.tsd.q
        ##TSD using time series log-linear model using indicator variables for strata
##with over-dispersion
 ### the estimator should be the same as obtained using model.tsd.clr, standard error will differ
 ################################################################################################

        fit.tsd.q=model.tsd.q(chic75s)
        b.tsd.q=fit.tsd.q$coeff[2]*1000
        se.tsd.q=summary(fit.tsd.q)$coeff[2,][[2]]*1000
        ##overdispersion parameter
        ##these two methods should give the same overdispersion 
        overdisp.tsd=(se.tsd.q/se.tsd)^2
       overdisp.tsd.q=(summary(fit.tsd.q))$dispersion              
        
       ### estimate beta using model.ns
       ## time series log-linear model using smooth function of day with 
       ##dow*natural spline  df=4*number years
 ##with over-dispersion

    fit.ns=model.ns(chic75s)
   b.ns=fit.ns$coeff[2]*1000
   se.ns=summary(fit.ns)$coeff[2,][[2]]*1000
    overdisp.ns=(summary(fit.ns))$dispersion   
    
##calculate dffit to check highly influential points
dffit.tsd=dffits(fit.tsd)
dffit.ns=dffits(fit.ns)


## exclude data between 19950714 and 19950718 due to high temperature.
data4=chic75s 
data4$death=replace(data4$death, data4$date>=19950714&chic75s$date<=19950718, NA)


fit4.tsd.clr=model.tsd.clr(data4)
b4.tsd.clr=fit4.tsd.clr$coeff[1]*1000
se4.tsd.clr=sqrt(fit4.tsd.clr$var[1])*1000 

fit4.tsd=model.tsd(data4)
b4.tsd=fit4.tsd$coeff[2]*1000
   se4.tsd=summary(fit4.tsd)$coeff[2,][[2]]*1000
dffit4.tsd=dffits(fit4.tsd)

fit4.tsd.q=model.tsd.q(data4)
b4.tsd.q=fit4.tsd.q$coeff[2]*1000
   se4.tsd.q=summary(fit4.tsd.q)$coeff[2,][[2]]*1000
    overdisp4.tsd=(summary(fit4.tsd.q))$dispersion              
    
       
  fit4.ns=model.ns(data4)
b4.ns=fit4.ns$coeff[2]*1000
   se4.ns=summary(fit4.ns)$coeff[2,][[2]]*1000
dffit4.ns=dffits(fit4.ns)
   
    overdisp4.ns=(summary(fit4.ns))$dispersion              
      
   
chic75s$dffit4.tsd=c(dffit4.tsd[1:194],rep(NA,5),dffit4.tsd[195:726])


##################################
#######Figure 3
#######Model checking using Dffits
###############################

par(mfrow=c(2,2), pin=c(2,2))

plot(dffit.tsd, xlab="", ylab="Dffits", 
 xaxt="n", main="Dffits for Method A",ylim=c(-3,7),frame.plot=F)
axis(1,at=c(1,182, 366,548, 732),lab=c("","","","", ""))

plot(dffit.ns, xlab="", ylab="Dffits", 
 xaxt="n", main="Dffits for Method D",ylim=c(-3,7),frame.plot=F)
axis(1,at=c(1,182, 366,548, 732),lab=c("","","","", ""))

plot(dffit4.tsd, xlab="Calendar Day", ylab="Dffits", 
 xaxt="n", main="Refitted Method A",ylim=c(-2,2),frame.plot=F)
axis(1,at=c(1,182, 366,548, 732),lab=c("Jan 95","Jul 95","Jan 96","Jul 96", "Jan 97"),cex=0.5)

plot(dffit4.ns, xlab="Calendar Day", ylab="Dffits", 
 xaxt="n", main="Refitted Method D",ylim=c(-2,2),frame.plot=F)
axis(1,at=c(1,182, 366,548, 732),lab=c("Jan 95","Jul 95","Jan 96","Jul 96", "Jan 97"),cex=0.5)
dev.off()
###########################################


####################################
##Figure 4
#######Model checking using Q-Q plot
################################

par(mfrow=c(2,2), pin=c(2,2))

      
   ## Q-Q plot for tsd (y-mu)/sqrt(mu)
   junk=na.omit(chic75s$death)-fit.tsd$fitted
   qqnorm(junk/sqrt(fit.tsd$fitted),main="Q-Q plot for Method A",ylim=c(-5,12.8),frame.plot=F, 
 xaxt="n",xlab="")
 axis(1,at=-3:3,lab=c("","","","","","",""))
abline(0,1)
## Q-Q plot for ns (y-mu)/sqrt(mu)
   junk2=na.omit(chic75s$death)-fit.ns$fitted
   qqnorm(junk2/sqrt(fit.ns$fitted),main="Q-Q plot for Method D",ylim=c(-5,12.8),frame.plot=F, 
 xaxt="n",xlab="")
 axis(1,at=-3:3,lab=c("","","","","","",""))
abline(0,1)
     
   ## Q-Q plot for tsd (y-mu)/sqrt(mu)

   qqnorm((na.omit(data4$death)-fit4.tsd$fitted)/sqrt(fit4.tsd$fitted),main="Refitted Method A",ylim=c(-3.5,3.5),frame.plot=F)
abline(0,1)
## Q-Q plot for ns (y-mu)/sqrt(mu)
     qqnorm((na.omit(data4$death)-fit4.ns$fitted)/sqrt(fit4.ns$fitted),main="Refitted Method D",ylim=c(-3.5,3.5),frame.plot=F)
abline(0,1)
dev.off()
########################


##dow 1=Sunday, ..., 7=Saturday

chic75s.1=chic75s[chic75s$dow==1,]
chic75s.2=chic75s[chic75s$dow==2,]
chic75s.3=chic75s[chic75s$dow==3,]
chic75s.4=chic75s[chic75s$dow==4,]
chic75s.5=chic75s[chic75s$dow==5,]
chic75s.6=chic75s[chic75s$dow==6,]
chic75s.7=chic75s[chic75s$dow==7,]

###############
##Figure 5
##checking standardized residuals 
##############
plot(chic75s.1$strata-0.3,chic75s.1$res.tsd, xlab="Day in Strata", ylab="Standardized Residuals", xlim=c(0,6)
,ylim=c(-5,15),cex=0.7,pch=1,xaxt="n",frame.plot=F)
points(chic75s.2$strata-0.2,chic75s.2$res.tsd, pch=2,cex=0.5)
points(chic75s.3$strata-0.1,chic75s.3$res.tsd,pch=3,cex=0.5)
points(chic75s.4$strata,chic75s.4$res.tsd,pch=4,cex=0.5)
points(chic75s.5$strata+0.1,chic75s.5$res.tsd,pch=5,cex=0.5)
points(chic75s.6$strata+0.2,chic75s.6$res.tsd,pch=6,cex=0.5)
points(chic75s.7$strata+0.3,chic75s.7$res.tsd,pch=7,cex=0.5)
legend(0,15,pch=1:7, c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
axis(1,at=1:5,lab=c("1","2","3","4", "5"),cex=0.5)
dev.off()





          
