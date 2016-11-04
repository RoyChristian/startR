paramsCI <- function(x){
  m <- mean(x)
  qt <- quantile(x,prob=c(0.025,0.975))
  out <- c(m,qt)
  names(out) <- c("y","ymin","ymax")
  return(out)
}
