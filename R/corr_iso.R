corr_iso <- function(E=0.01,P=0.05,D=diag(10)){
  N=nrow(D)
  K= matrix(-999, ncol=N, nrow=N)
  for (i in 1:(N-1)){
    for (j in (i+1):N) {
      K[i,j] <- exp(-P^2 * D[i,j]^2);
      K[j,i] <- K[i,j];
    }}
  for (k in 1:N){K[k,k] <- 1+ E^2;}

  return(K);
}
