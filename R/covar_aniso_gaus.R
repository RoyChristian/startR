#' Gaussian anisotropic covariance function
#'
#' @param sigma standard deviation for the covariance function
#' @param eta smoothness parameters. Control how smooth the covariance function is.
#' @param phi1 decay parameter. Control the strength of the covariance between the points.
#' @param phi2 decay parameter. Control the strength of the covariance between the points.
#' @param theta angle of the anisotropic field.
#' @param coords matrix holding the coordinates of the points. Remember that the points must be projected if you want a correaltion matrix that make sense.
#'
#' @return A Gaussian anisotropic covariance matrix between points.
#' @export
covar_aniso <- function(sigma=1.25, eta= 0.001, phi1=5,phi2=2.5, theta=0, coords=expand.grid(x=seq(-10,10,by=2), y=seq(-10,10, by=2))){
  inv.phi.mat  <- matrix(c(1/(phi1^2),0,0,1/(phi2^2)), 2,2)
  theta.mat <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), 2,2)
  lambda.mat <- t(theta.mat)%*%inv.phi.mat%*%theta.mat
  dist.array <- dist_func(coords)
  out <- matrix(-999, dim(dist.array)[1], dim(dist.array)[1])
  for(i in 1:nrow(out)){for(j in 1:ncol(out)){out[i,j] <- sigma^2 * exp(-t(dist.array[i,j,])%*%lambda.mat%*%dist.array[i,j,])}}

  diag(out) <- diag(out) + eta

  return(out)
}
