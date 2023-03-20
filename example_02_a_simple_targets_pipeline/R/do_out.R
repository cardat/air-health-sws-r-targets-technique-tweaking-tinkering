# tar_load(indat1)
# tar_load(indat2)
do_out <- function(
    i1 = indat1
    ,
    i2 = indat2
){
  foo <- data.frame(x=i1, y=i2)
  return(foo)
}