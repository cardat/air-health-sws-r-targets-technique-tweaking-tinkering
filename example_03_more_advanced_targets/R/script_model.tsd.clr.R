## Time-stratified case-crossover design (TSD)
##reference window: the same day of the week in the same month and year
####fit TSD using conditional logistic regression
##rearrange the data

data=chic75s
data=na.omit(data)
N=dim(data)[1]
##num.strata is the number of days in the strata
num.strata=as.vector(table(data$dowmonyr))

o = order(data$dowmonyr)
death.o=data$death[o]
pm10tmean.o=data$pm10tmean[o]
tmpd.o=data$tmpd[o]
dptp.o=data$dptp[o]

## m is the number of strata
m=length(num.strata)
death=NULL
wt=NULL
pm10=NULL
strata.clr=NULL
tmpd=NULL
dptp=NULL
str=1:N
for (i in 1:m)
{
  Y=diag(num.strata[i])
  death=c(death,as.vector(Y))
  tmp.wt=rep(death.o[(sum(num.strata[0:(i-1)])+1):sum(num.strata[1:i])],each=num.strata[i])
  wt=c(wt,tmp.wt)
  tmp.pm10=rep(pm10tmean.o[(sum(num.strata[0:(i-1)])+1):sum(num.strata[1:i])],num.strata[i])
  pm10=c(pm10,tmp.pm10)
  tmp.tmpd=rep(tmpd.o[(sum(num.strata[0:(i-1)])+1):sum(num.strata[1:i])],num.strata[i])
  tmpd=c(tmpd,tmp.tmpd)
  tmp.dptp=rep(dptp.o[(sum(num.strata[0:(i-1)])+1):sum(num.strata[1:i])],num.strata[i])
  dptp=c(dptp,tmp.dptp)
  tmp.str=rep(str[(sum(num.strata[0:(i-1)])+1):sum(num.strata[1:i])],each=num.strata[i])
  strata.clr=c(strata.clr,tmp.str)
}


fit.tsd.clr=coxph(Surv(rep(1,length(death)),death)~pm10+strata(strata.clr)+ns(tmpd,3)+ns(dptp,3),weights=wt)

