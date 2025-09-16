if(file.exists("example_02_a_simple_targets_pipeline")) setwd("example_02_a_simple_targets_pipeline") ## check if we are in the sub-folder, and move if not
library(targets)

if(!file.exists("figures_and_tables")) dir.create("figures_and_tables")

sapply(dir("R/", pattern = ".R$", full.names = T), source)

tar_visnetwork(targets_only = T)

tar_make()

#### exploratory data analysis ####
tar_load(out)
str(out)
