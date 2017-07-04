#' Look if a value is within an interval
#'
#' @param x value to test
#' @param limit.A lower limit
#' @param limit.B upper limit
#'
#' @return
#' @export
check_interval <- function(x, limit.A, limit.B){(limit.A < x & x < limit.B) | (limit.B <x & x < limit.A)}


