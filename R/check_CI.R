#'Check if zero is within the Credible intervals
#'
#' @param x vector holding the parameter draw from a MCMC of a MCMC or HMCMC sampler
#' @param probability quantile for the Creidble interval. Function only support 90 or 95 percent intervals.
#'
#' @return a charchter string that indicate whether the effect is signifcant or not
#' @export
#' @note From a Bayesian point of view this a very bad idea... use at your own risk.
#'
#' @examples
#'foo <- rnorm(1e4, 0.653, 0.192)
#'check_CI(foo, prob=0.95)
#'check_CI(bob, prob=0.90)
#'# apply on a matrix
#'bar <- matrix(rnorm(1e3*10,0.653, 0.192), ncol=10)
#'apply(bar, 2, check_CI, prob=0.95)
#'#END
check_CI <- function(x, prob=0.95){

  ###Make sure arguments works
  if(!prob %in% c(0.90,0.95))
    stop("Credible interval must be 90 or 95%")

  qt.x= params_CI(x,prob)
  test.int = check_intervall(0,qt.x[2],qt.x[3])
  out = ifelse(test.int,"non-signifcant","signifcant")
  return(out)
}
