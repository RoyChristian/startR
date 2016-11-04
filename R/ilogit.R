#' Logit and inverse logit
#'
#' @description logit compute the logit function while ilogit estimate the  inverse-logit of a number
#'
#' @param x a real number
#'
#' @return  a nuber between 0 and 1
#' @export
#'
#' @examples
#' logit(0.90)
#' ilogit(2.20)
#'#END
ilogit <- function(x){
  exp(x)/(1+exp(x))
}
