## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(dbscan)

## ----install, eval=FALSE------------------------------------------------------
# install.packages("dbscan")

## ----load-package-------------------------------------------------------------
library(dbscan)

## ----data---------------------------------------------------------------------
data("moons")
x <- as.matrix(moons)

plot(x, pch = 19, asp = 1, main = "Moons data")

## ----fit-dbscan---------------------------------------------------------------
cl <- dbscan(x, eps = 0.45, minPts = 5)
cl

## ----inspect-dbscan-----------------------------------------------------------
table(cl$cluster)
head(cl$cluster)

## ----add-labels---------------------------------------------------------------
clustered <- transform(moons, cluster = factor(cl$cluster))
head(clustered)

## ----plot-dbscan--------------------------------------------------------------
plot(
  x,
  col = cl$cluster + 1L,
  pch = ifelse(cl$cluster == 0, 4, 19),
  asp = 1,
  xlab = "X",
  ylab = "Y",
  main = "DBSCAN clustering"
)

## ----scaling, eval=FALSE------------------------------------------------------
# x <- scale(my_data)

## ----knn-distance-------------------------------------------------------------
kNNdistplot(x, minPts = 5)
abline(h = 0.45, col = 2, lty = 2)

## ----sensitivity--------------------------------------------------------------
settings <- c(0.40, 0.45, 0.50)
fits <- lapply(
  settings,
  function(e) dbscan(x, eps = e, minPts = 5)
)

data.frame(
  eps = settings,
  clusters = vapply(fits, function(fit) max(fit$cluster), integer(1)),
  noise = vapply(fits, function(fit) sum(fit$cluster == 0), integer(1))
)

## ----predict------------------------------------------------------------------
new_points <- rbind(
  c(0.0, 0.0),
  c(1.0, 2.0),
  c(4.0, 4.0)
)

predict(cl, newdata = new_points, data = x)

## ----hdbscan------------------------------------------------------------------
hdb <- hdbscan(x, minPts = 5)
hdb

plot(
  x,
  col = hdb$cluster + 1L,
  pch = ifelse(hdb$cluster == 0, 4, 19),
  asp = 1,
  main = "HDBSCAN clustering"
)

## ----optics-------------------------------------------------------------------
opt <- optics(x, minPts = 5)
plot(opt)

opt_cl <- extractDBSCAN(opt, eps_cl = 0.45)
table(opt_cl$cluster)

