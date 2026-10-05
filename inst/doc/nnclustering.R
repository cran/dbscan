## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(dbscan)

## ----data---------------------------------------------------------------------
data("DS3")
x <- as.matrix(DS3)

plot(x, pch = 19, cex = 0.25, asp = 1, main = "DS3 data")

## ----jp-fit-------------------------------------------------------------------
jp <- jpclust(x, k = 20, kt = 12)
c(clusters = ncluster(jp), noise = nnoise(jp))

## ----jp-result----------------------------------------------------------------
names(jp)
jp$param
head(jp$cluster)

## ----jp-plot------------------------------------------------------------------
clplot(x, jp, cex = 0.25, main = "Jarvis-Patrick clustering")

## ----jp-sensitivity-----------------------------------------------------------
jp_settings <- c(10, 12, 14)
jp_fits <- lapply(
  jp_settings,
  function(threshold) jpclust(x, k = 20, kt = threshold)
)

data.frame(
  kt = jp_settings,
  clusters = vapply(jp_fits, ncluster, integer(1)),
  singleton_clusters = vapply(
    jp_fits,
    function(fit) sum(table(fit$cluster) == 1L),
    integer(1)
  )
)

## ----snn-fit------------------------------------------------------------------
snn <- sNNclust(x, k = 20, eps = 7, minPts = 16)
snn

## ----snn-plot-----------------------------------------------------------------
clplot(x, snn, cex = 0.25, main = "Shared nearest neighbor clustering")

## ----snn-sensitivity----------------------------------------------------------
snn_settings <- data.frame(
  eps = c(5, 7, 9),
  minPts = c(16, 16, 16)
)

snn_fits <- lapply(
  seq_len(nrow(snn_settings)),
  function(i) sNNclust(
    x,
    k = 20,
    eps = snn_settings$eps[i],
    minPts = snn_settings$minPts[i]
  )
)

transform(
  snn_settings,
  clusters = vapply(snn_fits, ncluster, integer(1)),
  noise = vapply(snn_fits, nnoise, integer(1))
)

## ----reuse-knn----------------------------------------------------------------
nn <- kNN(x, k = 30)

jp_from_nn <- jpclust(nn, k = 20, kt = 12)
snn_from_nn <- sNNclust(nn, k = 20, eps = 7, minPts = 16)

c(
  jp_same = identical(jp$cluster, jp_from_nn$cluster),
  snn_same = identical(snn$cluster, snn_from_nn$cluster)
)

## ----inspect-snn--------------------------------------------------------------
shared_nn <- sNN(nn, k = 20, jp = TRUE, sort = FALSE)
shared_nn
table(shared_nn$shared)

## ----other-distance, eval=FALSE-----------------------------------------------
# d <- dist(my_data, method = "manhattan")
# 
# jp_manhattan <- jpclust(d, k = 20, kt = 12)
# snn_manhattan <- sNNclust(d, k = 20, eps = 7, minPts = 16)

