frNN <- function(x, eps, sort = TRUE, search = "kdtree", bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  search <- pmatch(toupper(search), c("KDTREE", "LINEAR"))
  if(is.na(search)) stop("Unknown NN search type!")

  ret <- frNN_int(as.matrix(x), as.double(eps),
    as.integer(search), as.integer(bucketSize),
    as.integer(splitRule), as.double(approx))

  if(sort) {
    o <- lapply(ret$dist, order, decreasing=FALSE)
    for(i in 1:length(o)) {
      ret$dist[[i]] <- ret$dist[[i]][o[[i]]]
      ret$id[[i]] <- ret$id[[i]][o[[i]]]
      }
  }

  ret
}
