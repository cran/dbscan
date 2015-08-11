
lof <- function(x, k = 4, ...) {
  n <- nrow(x)
  if(k<1 || k>=n)
    stop("k has to be larger than 1 and smaller than the number of points")

  # get k nearest neighbors + distances
  d <- kNN(x, k, ...)

  # calculate local reachability density
  lrd <- numeric(n)
  for(i in 1:n) lrd[i] <- 1/(sum(apply(
    cbind(d$dist[d$id[i,], k], d$dist[i,]),
    1, max)) / k)

  # calculate lof
  lof <- numeric(n)
  for (i in 1:n) lof[i] <- sum(lrd[d$id[i,]])/k / lrd[i]

  lof
}
