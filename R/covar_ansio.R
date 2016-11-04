corr_aniso <- function(phi1=1000,phi2=3000, theta=0, coords=expand.grid(x=seq(-100,100,by=20),y=seq(-100,100,by=20))){
  inv.phi.mat  <- matrix(c(1/(phi1^2),0,0,1/(phi2^2)), 2,2)
  theta.mat <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), 2,2)
  lambda.mat <- t(theta.mat)%*%inv.phi.mat%*%theta.mat
  dist.array <- dist.func(coords)
  out <- matrix(-999, dim(dist.array)[1], dim(dist.array)[1])
  for(i in 1:nrow(out)){for(j in 1:ncol(out)){out[i,j] <- exp(-t(dist.array[i,j,])%*%lambda.mat%*%dist.array[i,j,])}}
  return(out)
}
