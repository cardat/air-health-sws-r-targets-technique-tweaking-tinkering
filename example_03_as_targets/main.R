## check if we are in the sub-folder, and move if not
if(file.exists("example_03_as_targets")) setwd("example_03_as_targets") 
library(targets)
tar_visnetwork(targets_only = T)
tar_make()


#### Exploratory interrogation ####
perform_EDA <- FALSE
if(perform_EDA){
  #### _QA_QC ####
  
  
  #### _figures ####
  
  
  #### _tables ####
  
}