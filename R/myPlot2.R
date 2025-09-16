# tar_load(out)
myPlot2 <- function(out, filename){
  out$ybins <- cut(out$y, breaks = quantile(out$y, probs = seq(0,1,.1)), include.lowest = T, labels = paste0(1:10, "_group"))
  # table(out$ybins)
  # with(out, boxplot(y ~ ybins))
  setDT(out)
  out2 <- out[,.(y_mean = mean(y), y_sdev = sd(y)), by = ybins][order(ybins)]
  png(filename)
  with(out2, plotCI(1:nrow(out2), y_mean, uiw = y_sdev))
  dev.off()
}