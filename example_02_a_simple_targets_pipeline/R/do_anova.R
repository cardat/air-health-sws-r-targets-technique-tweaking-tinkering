do_anova <- function(lm_model, filename){
  png(filename)
  par(mfrow = c(2,2)) # 2x2 grid
  plot(lm_model)
  dev.off
}