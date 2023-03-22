
##fit TSD using time series log-linear model using indicator variables for strata
##without over-dispersion
model.tsd=function(data)
{
  fit.tsd=glm(death~pm10tmean+as.factor(dowmonyr)+ns(tmpd,3)+ns(dptp,3),data=data,family=poisson)
  return(fit.tsd)
}
