###internal function for the covar and correlation anisotropic function


dist_func<-function(Coords=Coords){
  dist.list <- lapply(1:nrow(Coords), function(i){
    a <- Coords[i,1] - Coords[,1]
    b <- Coords[i,2] - Coords[,2]
    return(rbind(a,b))
  })
  dist.array  <- array(unlist(dist.list), dim = c(nrow(dist.list [[1]]), ncol(dist.list [[1]]), length(dist.list)))
  dist.perm <- aperm(dist.array, c(2,3,1))
  return(dist.perm)
}
