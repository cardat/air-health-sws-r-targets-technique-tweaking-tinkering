inV2 <- function(indat2, min_val=1, max_val=20){
  indatV2 <- indat2[indat2 >= min_val & indat2 <= max_val]
  
  if(length(indatV2) == 0){
    warning('The values of indat2 must align with a specified range [", min_val,", ", max_val, "]')
  }
  return(indatV2)
}