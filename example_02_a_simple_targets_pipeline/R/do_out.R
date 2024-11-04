# tar_load(indat1)
# tar_load(indat2)
# tar_load(indatV2)
do_out <- function(
    i1 = indat1
    ,
    i2 = indatV2
){
  foo <- data.frame(x=i1, y=i2)
  return(foo)
}
