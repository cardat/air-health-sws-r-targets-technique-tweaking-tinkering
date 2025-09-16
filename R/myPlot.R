myPlot <- function(out, filename){
  png(filename)
  with(out, plot(x, y))
  dev.off()
}