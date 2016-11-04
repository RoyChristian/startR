#' Logit and inverse logit
#'
#' @description logit compute the logit function while ilogit estimate the  inverse-logit of a number
#'
#' @param x a nuber between 0 and 1
#'
#' @return a real number
#' @export
#'
#' @examples
#' logit(0.90)
#' ilogit(2.20)
#'#END
logit <- function(x){
  log(x/(1-x))
}
