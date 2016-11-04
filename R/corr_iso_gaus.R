#' Gaussian isotropic correlation function
#'
#' @description Calulate a Gaussian correlation functions based on a vector of coordinates.Remember that the points must be projected if you want a correaltion matrix that make sense.
#'
#'
#' @param eta smoothness parameters. Control how smooth the correlation function is.
#' @param phi decay parameters. Control the strength of the correlation between the points.
#' @param coords matrix holding the coordinates of the points.
#' @return a Gaussian isotropic correlation matrix between points.
#' @export
#'
#' @examples
corr_iso_gaus <- function(eta=0.01,phi=5, coords=expand.grid(x=seq(-10,10,by=2), y=seq(-10,10, by=2))){

  dist.mat <- as.matrix(dist(coords))

  dist.mat <- as.matrix(dist(coords))

  N=nrow(dist.mat)
  K= exp(- (1/(2*phi^2)) * dist.mat^2)
  diag(K) <- 1 + eta^2

  return(K);
}
