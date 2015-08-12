kNN <- function(x, k, sort = TRUE, search = "kdtree", bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  k <- as.integer(k)

  if(k < 1) stop("Illegal k: needs to be k>=1!")
  if(k >= nrow(x)) stop("Not enought neighbors in data set!")

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  search <- pmatch(toupper(search), c("KDTREE", "LINEAR"))
  if(is.na(search)) stop("Unknown NN search type!")

  ret <- kNN_int(as.matrix(x), as.integer(k),
    as.integer(search), as.integer(bucketSize),
    as.integer(splitRule), as.double(approx))

  ### sort entries?
  if(sort && k>1) {
    o <- apply(ret$dist, 1, order, decreasing=FALSE)
    for(i in 1:ncol(o)) {
      ret$dist[i,] <- ret$dist[i,][o[,i]]
      ret$id[i,] <- ret$id[i,][o[,i]]
      }
  }

  ret
}
