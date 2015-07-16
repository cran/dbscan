dbscan <- function(x, eps, minPts = 5, bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  dbscan_int(as.matrix(x), as.double(eps), as.integer(minPts),
    as.integer(bucketSize), as.integer(splitRule), as.double(approx))
}
