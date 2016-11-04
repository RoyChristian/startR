#' Gaussian isotropic covariance function
#'
#' @description Calulate a Gaussian covariance functions based on a vector of coordinates.Remember that the points must be projected if you want a correaltion matrix that make sense.
#'
#' @param sigma standard deviation for the covariance function
#' @param eta smoothness parameters. Control how smooth the correlation function is.
#' @param phi decay parameters. Control the strength of the correlation between the points.
#' @param coords matrix holding the coordinates of the points.
#'
#' @return A Gaussian isotropic covariance matrix between points.
#' @export
#'
#' @examples
covar_iso_gaus <- function(sigma= 1.25, eta=0.01,phi=5, coords=expand.grid(x=seq(-10,10,by=2), y=seq(-10,10, by=2))){

  dist.mat <- as.matrix(dist(coords))

  N=nrow(dist.mat)
  K=  sigma^2 * exp(- (1/(2*phi^2)) * dist.mat^2)
  diag(K) <- sigma^2 + eta^2

  return(K);
}
