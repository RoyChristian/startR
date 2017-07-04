#' Gaussian anisotropic correlation function
#'
#' @param eta smoothness parameters. Control how smooth the correlation function is.
#' @param phi1 decay parameter. Control the strength of the correlation between the points.
#' @param phi2 decay parameter. Control the strength of the correlation between the points.
#' @param theta angle of the anisotropic field.
#' @param coords matrix holding the coordinates of the points. Remember that the points must be projected if you want a correaltion matrix that make sense.
#'
#' @return A Gaussian anisotropic correlation matrix between points.
#' @export
corr_aniso <- function(eta= 0.001, phi1=5,phi2=2.5, theta=0, coords=expand.grid(x=seq(-10,10,by=2), y=seq(-10,10, by=2))){
  inv.phi.mat  <- matrix(c(1/(phi1^2),0,0,1/(phi2^2)), 2,2)
  theta.mat <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), 2,2)
  lambda.mat <- t(theta.mat)%*%inv.phi.mat%*%theta.mat
  dist.array <- dist_func(coords)
  out <- matrix(-999, dim(dist.array)[1], dim(dist.array)[1])
  for(i in 1:nrow(out)){for(j in 1:ncol(out)){out[i,j] <- exp(-t(dist.array[i,j,])%*%lambda.mat%*%dist.array[i,j,])}}

  diag(out) <- diag(out) + eta

  return(out)
}
