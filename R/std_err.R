#' Standard error
#'
#' @param x vector containing repeated measurement
#'
#' @return a real number representing the standard error
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' mean(x)
#' std_err(x)
#' #End
std_err <- function(x){
  sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}
