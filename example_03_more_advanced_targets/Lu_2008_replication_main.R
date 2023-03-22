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
## Time-stratified case-crossover design (TSD)
##reference window: the same day of the week in the same month and year
### estimate beta using model.tsd.clr
####TSD using conditional logistic regression
source("R/script_model.tsd.clr.R")
summary(fit.tsd.clr)
##b.tsd.clr is the percentage increase of relative risk per 10microgram pm10 increase
b.tsd.clr = (exp(fit.tsd.clr$coeff[1] * 10) - 1) * 100
## the following is the Increase in risk and 95%CI of the estimate
x <- summary(fit)$coeff
nms <- rownames(x)
beta <- x[which(nms %in% subset),1]
se <- x[which(nms %in% subset),2]
est <- exp(beta)
x2p5 <- exp(beta) - (1.96 * se)
x97p5 <- exp(beta) + (1.96 * se)

source("R/script_model.tsd.R")


#### second option: R-targets pipeline ####
library(targets)

if(!file.exists("figures_and_tables")) dir.create("figures_and_tables")

sapply(dir("R/", pattern = ".R$", full.names = T), source)

tar_visnetwork(targets_only = T)

tar_make()

#### exploratory data analysis ####
tar_load(out)
str(out)
