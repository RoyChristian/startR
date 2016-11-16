#' Coefficient of variation
#'
#' @param x a numeric vector or an R object which is coercible to one by as.double(x)
#' @param na.rm  logical. Should missing values be removed?
#'
#' @return the coeficient of variation
#' @export
#'
#' @examples
#' foo <- rnorm(100,10,10)
#' sd(foo)/mean(foo)
#' cv(foo)
cv <- function(x, na.rm=FALSE){sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)}
