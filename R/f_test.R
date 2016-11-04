f_test <- function(x){
  mean.x <- mean(x)

  if(mean.x>0){
    out <- mean(x>0)
  }else{
    out <- mean(x<0)
  }
  return(out)
}
