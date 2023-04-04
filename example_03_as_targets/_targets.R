library(targets)
#### settings ####
list(
  #### Data Integration ####
  #### _dat_gcc ####
  tar_target(dat_gcc,
             do_load_gcc(input_filename)
             )
  ,
  tar_target(dat_derived,
             do_derivation(argument = dat_gcc)
             )
  ,
  tar_target(dat_exposure,
             do_exposure()
             )
  ,
  tar_target(merge,
             do_merge(dat_derived, dat_exposure)
             )
  #,
  #### Data cleaning ####
  
  #### Main Analysis ####
  
  #### Sensitivity ####
  
)