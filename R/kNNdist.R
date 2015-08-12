kNNdist <- function(x, k, search = "kdtree", bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  if(k >= nrow(x)) stop("Not enought neighbors in data set!")

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  search <- pmatch(toupper(search), c("KDTREE", "LINEAR"))
  if(is.na(search)) stop("Unknown NN search type!")

  kNNdist_int(as.matrix(x), as.integer(k),
    as.integer(search), as.integer(bucketSize),
    as.integer(splitRule), as.double(approx))
}

kNNdistplot <- function(x, k = 4, ...) {
  kNNdist <- sort(kNNdist(x, k ,...))
  plot(sort(kNNdist), type="l", ylab=paste(k, "-NN distance", sep=""),
    xlab = "Pointes (sample) sorted by distance")
}
