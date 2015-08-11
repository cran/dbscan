optics <- function(x, eps, minPts = 5, eps_cl, search = "kdtree", bucketSize = 10,
  splitRule = "suggest", approx = 0) {

  splitRule <- pmatch(toupper(splitRule),
    c("STD", "MIDPT", "FAIR", "MIDPT", "SL_FAIR", "SUGGEST"))-1L
  if(is.na(splitRule)) stop("Unknown splitRule!")

  search <- pmatch(toupper(search), c("KDTREE", "LINEAR"))
  if(is.na(search)) stop("Unknown NN search type!")

  ret <- optics_int(as.matrix(x), as.double(eps), as.integer(minPts),
    as.integer(search), as.integer(bucketSize),
    as.integer(splitRule), as.double(approx))

  ### find clusters
  if(!missing(eps_cl)) {
    reachdist <- ret$reachdist[ret$order]
    coredist <- ret$coredist[ret$order]
    n <- length(ret$order)
    cluster <- integer(n)
    clusterid <- 0L
    for(i in 1:n) {
      if(reachdist[i] > eps_cl) {
        if(coredist[i] <= eps_cl) {
          clusterid <- clusterid + 1L
          cluster[i] <- clusterid
         }else{
          cluster[i] <- 0L ### noise
        }
    #    cluster[i] <- 0L ### noise
      }else{
     #   if(i>1 && cluster[i-1]==0L) clusterid <- clusterid + 1L
        cluster[i] <- clusterid
      }
    }

    ret$eps_cl <- eps_cl

    ### fix the order so cluster is in the same order as the rows in x
    cluster[ret$order] <- cluster
    ret$cluster <- cluster
  }


  ret$eps <- eps
  ret$minPts <- minPts

  class(ret) <- "optics"
  ret
}


print.optics <- function(x, ...) {
  cat("OPTICS clustering for ", length(x$order), " objects.", "\n", sep = "")
  cat("Parameters: eps = ", x$eps, ", minPts = ", x$minPts, "\n", sep = "")
  if(!is.null(x$cluster)) {
    cl <- unique(x$cluster)
    cl <- length(cl[cl!=0L])
    cat("The clustering contains ", cl, " cluster(s).",
      "\n", sep = "")
  }
  cat("Available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
  }

plot.optics <- function(x, y=NULL, ...) {
    if(!is.null(x$cluster)) {
      plot(x$reachdist[x$order], type="h", col=x$cluster[x$order]+1L,
        ylab = "Reachability dist.", xlab = "OPTICS order",
        main = "Reachability Plot")
      # abline(h=x$eps_cl, col="gray", lty=2)
    }else{
      plot(x$reachdist[x$order], type="h",
        ylab = "Reachability dist.", xlab = "OPTICS order",
        main = "Reachability Plot")
    }
}
