## check if we are in the sub-folder, and move if not
if(file.exists("example_03_more_advanced_targets")) setwd("example_03_more_advanced_targets") 

#### IVAN HANIGAN ADAPTED FROM 
##### epiweb.R 
##### Yun LU 
##### 09/07/2007

##model-checking sample code for case-crossover analyses
library(survival)
## load splines library in order to fit natural spline
## for temperature and dew point temperature
library(splines)

#### First option: standard scripted workflow ####
source("R/script_load_the_data.R")
source("R/script_plot_time_series.R")
#### Time-stratified case-crossover design (TSD) ####
##reference window: the same day of the week in the same month and year
### estimate beta using model.tsd.clr
####TSD using conditional logistic regression
source("R/script_model.tsd.clr.R")
fittedc <- summary(fit.tsd.clr)$coeff
fittedc
##b.tsd.clr is the percentage increase/decrease of relative risk per 10microgram pm10 increase
beta <- fittedc[1,"coef"]
se <- fittedc[1,"se(coef)"]
b.tsd.clr = (exp(beta * 10) - 1) * 100
##se.tsd.clr is the standard error of the estimated risk% per 10microgram pm10 change
se.tsd.clr=(exp(se * 10) - 1) * 100

## the following is a function to get the Increase in risk and 95%CI of the estimate
ci_95 <- function(
    fit = fit.tsd.clr,
    subset = "pm10",
    delta = 10
    ){
  x <- summary(fit)$coeff
  nms <- rownames(x)
  beta <- x[which(nms %in% subset),"coef"]
  se <- x[which(nms %in% subset),"se(coef)"]
  est <- (exp(beta * delta))#-1)*100 
  x2p5 <-  (exp((beta - 1.96 * se)*delta))# - 1)*100
  x97p5 <- (exp((beta + 1.96 * se)*delta))# - 1)*100
  estout <- data.frame(est, x2p5, x97p5)
  return(estout)
}
estimates <- ci_95(fit = fit.tsd.clr, subset = "pm10",delta = 10)
knitr::kable(estimates, digits = 5)

#### TODO model.tsd####
### estimate beta using model.tsd
##TSD using time series log-linear model using indicator variables for strata
##without over-dispersion
### the estimator and standard error should be the same as obtained using model.tsd.clr
### model.tsd allows for model-checking using log-linear model diagnostics
source("R/script_model.tsd.R")
## this currently just runs the function definition

#### TODO second option: R-targets pipeline ####
# library(targets)
# 
# if(!file.exists("figures_and_tables")) dir.create("figures_and_tables")
# 
# sapply(dir("R/", pattern = ".R$", full.names = T), source)
# 
# tar_visnetwork(targets_only = T)
# 
# tar_make()
# 
# #### exploratory data analysis ####
# tar_load(out)
# str(out)
