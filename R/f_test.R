#' f test for Bayesian parameters estimates
#'
#' @description Check what proportion of a parameter estimate fall on the same side of the mean.  This value can be use as reference of how confidnet your estimate of the parameter is.
#'
#' @param x vector holding the parameter draw from a MCMC of HMCMC sampler
#'
#' @return the proportion of the parmater estimate that is on the same side of the mean.
#' @export
#'
#' @examples
#' f_test(rnorm(1e5,mean=-3,sd=6)) #low confidence
#' f_test(rnorm(1e5,mean=-3,sd=2)) #high confidence
#' f_test(rnorm(1e5,mean=-3,sd=0.5)) #Certain
#' #END
f_test <- function(x){
  mean.x <- mean(x)

  if(mean.x>0){
    out <- mean(x>0)
  }else{
    out <- mean(x<0)
  }
  return(out)
}
