##fit TSD using time series log-linear model using indicator variables for strata
##It allows for overdispersion by using quasipoisson
## 
model.tsd.q=function(data)
{
  fit.tsd.q=glm(death~pm10tmean+as.factor(dowmonyr)+ns(tmpd,3)+ns(dptp,3),data=data,family=quasipoisson)
  return(fit.tsd.q)
}
