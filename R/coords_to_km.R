#' Geographic coordinates to kilometers coordinates
#'
#' @description Transform geographic coordinates into a distance based coordinates. The function is based on the Haversine formula.
#'
#' @param lon longitude vector in degrees
#' @param lat latitude vector in degrees
#' @param lon0 longitude of the centroid of the survey area
#' @param lat0 latittude of the centroid of the survey area
#'
#' @return Return a data frame holding the new coordinates
#'
#' @examples
#' deg.grid <- expand.grid(x=seq(40,50,by=2.5), y=seq(60,70,by=2.5))
#' km_.grid <- coords_to_km(lon=deg.grid$x, lat=deg.grid$y, lon0=mean(deg.grid$x), lat0=mean(deg.grid$y))
#' plot(km.grid)
#'#END
#' @export
coords_to_km <- function (lon, lat, lon0, lat0)
{
  R <- 6371
  deg2rad = pi/180
  rlon <- lon * deg2rad
  rlat <- lat * deg2rad
  rlat0 <- lat0 * deg2rad
  rlon0 <- lon0 * deg2rad
  delrlon <- rlon - rlon0
  km.n <- sign(rlat - rlat0) * acos(sin(rlat0) * sin(rlat) +
                                      cos(rlat0) * cos(rlat)) * R
  km.e <- sign(rlon - rlon0) * acos(sin(rlat) * sin(rlat) +
                                      cos(rlat) * cos(rlat) * cos(delrlon)) * R
  return(data.frame(km.e = km.e, km.n = km.n))
}
