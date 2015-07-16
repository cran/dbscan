
kNNdistplot <- function(x, k = 4) {
  NNdist <- as.matrix(dist(x))
  NNdist <- t(apply(NNdist, MARGIN = 1, sort))
  plot(sort(NNdist[,k]), type="l", ylab=paste(k, "-NN distance", sep=""),
    xlab = "Pointes (sample) sorted by distance")
}
