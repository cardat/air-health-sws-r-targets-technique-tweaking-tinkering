library(targets)
library(data.table)
sapply(dir("R/", pattern = ".R$", full.names = T), source)

list(
  #### indat1 ####
  tar_target(indat1,
             in1(n=2000)
             )
  ,
  #### indat2 ####
  tar_target(indat2,
             in2(n=2000)
             )
  ,
  #### out ####
  tar_target(out,
             do_out(i1=indat1, i2=indat2)
             )
  ,
  #### plt ####
  tar_target(plt,
             myPlot(out, filename = "figures_and_tables/plt.png")
             )
  ,
  #### plt2 ####
  tar_target(plt2,
             myPlot2(out, filename = "figures_and_tables/plt2.png")
             )
)
