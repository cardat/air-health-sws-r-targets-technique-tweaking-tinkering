### fit smooth funciton of day with dow*natural spline  df=4*number years, with overdispersion

model.ns=function(data)
{
  data$day=1:dim(data)[1]
  fit.ns=glm(death~pm10tmean+as.factor(dow):ns(day,df=8)+ns(tmpd,3)+ns(dptp,3),data=data,family=quasipoisson)
  return(fit.ns)
}
