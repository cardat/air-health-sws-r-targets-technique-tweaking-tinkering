# plot a simple linear regression
do_lm <- function(out, filename){
  # fit a linear model
  lm_model <- lm(y~x, data = out)
  # do scatter plot with regression line
  png(filename)
  plot(out$x, out$y, main = 'Simple linear regression', xlab = 'X', ylab = 'Y')
  abline(lm_model, col = 'red', lwd=2)
  dev.off()
  
  return(lm_model)
}