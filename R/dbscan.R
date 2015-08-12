dbscan <- function(x, eps, minPts = 5, borderPoints = TRUE, search = "kdtree", bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  search <- pmatch(toupper(search), c("KDTREE", "LINEAR"))
  if(is.na(search)) stop("Unknown NN search type!")

  ret <- dbscan_int(as.matrix(x), as.double(eps), as.integer(minPts),
    as.integer(borderPoints),
    as.integer(search), as.integer(bucketSize),
    as.integer(splitRule), as.double(approx))

  ret <- list(cluster = ret, eps = eps, minPts = minPts)
  class(ret) <- "dbscan"
  ret
}


print.dbscan <- function(x, ...) {
  cat("DBSCAN clustering for ", length(x$cluster), " objects.", "\n", sep = "")
  cat("Parameters: eps = ", x$eps, ", minPts = ", x$minPts, "\n", sep = "")
  cl <- unique(x$cluster)
  cl <- length(cl[cl!=0L])
  cat("The clustering contains ", cl, " cluster(s).",
      "\n", sep = "")
  cat("Available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
}

