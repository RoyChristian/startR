#' Title Degree angle to radian angle
#'
#' @description Transform degree angle to radian angle
#'
#' @param x angle value in degrees
#'
#' @return angle value in radian
#' @export
#'
#' @examples
#'  deg_to_rad(90)
#' #END
deg_to_rad<- function(x){x*pi/180}
