## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(dbscan)

## ----data---------------------------------------------------------------------
set.seed(2)
n <- 400
x <- cbind(
  x = runif(4, 0, 1) + rnorm(n, sd = 0.1),
  y = runif(4, 0, 1) + rnorm(n, sd = 0.1)
)

plot(x, pch = 19, asp = 1, main = "Example data")

## ----fit----------------------------------------------------------------------
opt <- optics(x, minPts = 10)
opt

## ----inspect------------------------------------------------------------------
head(opt$order, 10)
head(opt$reachdist[opt$order], 10)

## ----reachability-------------------------------------------------------------
plot(opt)

## ----order--------------------------------------------------------------------
plot(x, col = "grey70", pch = 19, asp = 1, main = "OPTICS order")
lines(x[opt$order, ], col = "grey40")
points(x[opt$order[1], , drop = FALSE], pch = 19, col = 2)

## ----extract-dbscan-----------------------------------------------------------
opt_db <- extractDBSCAN(opt, eps_cl = 0.065)
opt_db
plot(opt_db)

## ----plot-dbscan--------------------------------------------------------------
table(opt_db$cluster)

plot(
  x,
  col = opt_db$cluster + 1L,
  pch = ifelse(opt_db$cluster == 0, 4, 19),
  asp = 1,
  main = "Clusters extracted at eps_cl = 0.065"
)

## ----recut--------------------------------------------------------------------
opt_db_wide <- extractDBSCAN(opt, eps_cl = 0.07)
c(
  clusters = max(opt_db_wide$cluster),
  noise = sum(opt_db_wide$cluster == 0)
)

## ----extract-xi---------------------------------------------------------------
opt_xi <- extractXi(opt, xi = 0.05)
opt_xi
opt_xi$clusters_xi
plot(opt_xi)

## ----plot-xi------------------------------------------------------------------
plot(
  x,
  col = opt_xi$cluster + 1L,
  pch = ifelse(opt_xi$cluster == 0, 4, 19),
  asp = 1,
  main = "Top-level Xi cluster labels"
)

## ----minimum-xi---------------------------------------------------------------
opt_xi_min <- extractXi(opt, xi = 0.05, minimum = TRUE)
opt_xi_min

## ----dendrogram---------------------------------------------------------------
dend <- as.dendrogram(opt)
plot(dend, leaflab = "none", ylab = "Reachability distance")

## ----predict------------------------------------------------------------------
new_points <- rbind(
  c(0.2, 0.3),
  c(0.8, 0.8),
  c(2.0, 2.0)
)

predict(opt_db, newdata = new_points, data = x)

## ----distance, eval=FALSE-----------------------------------------------------
# d <- dist(x, method = "manhattan")
# opt_manhattan <- optics(d, minPts = 10)
# plot(opt_manhattan)

