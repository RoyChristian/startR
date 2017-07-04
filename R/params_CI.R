#' Function to estimate credible intervals
#'
#' @description Estimate the mean and confidence intervals at a given probability for for Bayesian parameters estimates
#'
#' @param x vector holding the parameter draw from a MCMC of a MCMC or HMCMC sampler
#' @param probability quantile for the Creidble interval. Function only support 90 or 95 percent intervals.
#'
#' @return a vector holding, the mean, the lower credible intervals and the upper credible intervals
#' @export
#'
#' @examples
#'foo <- rnorm(1e4, 0.653, 0.192)
#'paramsCI(foo, prob=0.95)
#'paramsCI(bob, prob=0.90)
#'# apply on a matrix
#'bar <- matrix(rnorm(1e3*10,0.653, 0.192), ncol=10)
#'as.data.frame(t(apply(bar, 2, paramsCI, prob=0.95)))
#'#END
params_CI <- function(x, prob=0.95){

  ###Make sure arguments works
  if(!prob %in% c(0.90,0.95))
  stop("Credible interval must be 90 or 95%")

  m <- mean(x)
  if(prob==0.95){
    qt <- quantile(x,prob=c(0.025,0.975))
  }else{
    qt <- quantile(x,prob=c(0.05,0.95))
  }
  out <- c(m,qt)
  names(out) <- c("y","ymin","ymax")
  return(out)
}
