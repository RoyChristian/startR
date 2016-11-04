coords_to_km <- function (lon, lat, lon0 = sum(range(lon))/2, lat0 = sum(range(lat))/2)
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
  return(list(km.e = km.e, km.n = km.n))
}
