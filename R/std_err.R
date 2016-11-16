#' Standard error
#'
#' @param a numeric vector or an R object which is coercible to one by as.double(x)
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
