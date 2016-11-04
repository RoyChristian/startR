#' Title Radial angle to degree angle
#'
#' @description Transform radian angle to degree angle
#'
#' @param x angle value in radian
#'
#' @return angle value in degrees
#' @export
#'
#' @examples
#'  rad_to_deg(2*pi)
#' #END
rad_to_deg <- function(x){x*(180/pi)}
