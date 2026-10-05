## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(dbscan)

## ----data---------------------------------------------------------------------
data("moons")
x <- as.matrix(moons)
rownames(x) <- paste0("p", seq_len(nrow(x)))

plot(x, pch = 19, asp = 1, main = "Moons data")

## ----knn----------------------------------------------------------------------
knn5 <- kNN(x, k = 5)
knn5

head(knn5$id)
head(knn5$dist)

## ----inspect-knn--------------------------------------------------------------
i <- 10
data.frame(
  id = knn5$id[i, ],
  row_name = rownames(x)[knn5$id[i, ]],
  distance = knn5$dist[i, ]
)

## ----knn-adjacency------------------------------------------------------------
knn_adj <- adjacencylist(knn5)
knn_adj[1:3]

## ----plot-knn-----------------------------------------------------------------
plot(knn5, x, pch = 19, main = "5-nearest-neighbor graph")

## ----reduce-knn---------------------------------------------------------------
knn10 <- kNN(x, k = 10)
knn3 <- kNN(knn10, k = 3)

c(stored = knn10$k, reduced = knn3$k)
head(knn3$id)

## ----knn-query----------------------------------------------------------------
query <- rbind(
  left = c(-0.5, 0.5),
  right = c(1.5, 0.5)
)

query_knn <- kNN(x, k = 4, query = query)
query_knn$id
query_knn$dist

## ----plot-query---------------------------------------------------------------
plot(query_knn, x, col = "grey70", main = "Neighbors of query points")
points(query, pch = 4, lwd = 2, cex = 1.3)

## ----frnn---------------------------------------------------------------------
fr <- frNN(x, eps = 0.25)
fr

fr$id[1:3]
fr$dist[1:3]
summary(lengths(fr$id))

## ----frnn-adjacency-----------------------------------------------------------
identical(adjacencylist(fr), fr$id)
which(lengths(fr$id) == 0L)

## ----plot-frnn----------------------------------------------------------------
plot(fr, x, pch = 19, main = "Fixed-radius graph (eps = 0.25)")

## ----reduce-frnn--------------------------------------------------------------
fr_wide <- frNN(x, eps = 0.30)
fr_small <- frNN(fr_wide, eps = 0.12)

c(wide_edges = sum(lengths(fr_wide$id)),
  small_edges = sum(lengths(fr_small$id)))

## ----frnn-query---------------------------------------------------------------
query_fr <- frNN(x, eps = 0.25, query = query)
data.frame(
  query = rownames(query),
  neighbors = lengths(query_fr$id)
)

## ----snn----------------------------------------------------------------------
snn <- sNN(x, k = 10)
snn

head(snn$id, 3)
head(snn$shared, 3)
table(snn$shared)

## ----threshold-snn------------------------------------------------------------
snn5 <- sNN(snn, kt = 5)
snn5$id[1:3, ]
adjacencylist(snn5)[1:3]

## ----mutual-snn---------------------------------------------------------------
snn_mutual <- sNN(knn10, k = 10, jp = TRUE)
c(
  all_edges = sum(!is.na(snn$id)),
  mutual_edges = sum(snn_mutual$shared > 0)
)

## ----knndist------------------------------------------------------------------
d5 <- kNNdist(x, k = 5)
head(d5)
summary(d5)

## ----knndist-all--------------------------------------------------------------
d_all <- kNNdist(x, k = 5, all = TRUE)
head(d_all)

## ----knndistplot--------------------------------------------------------------
kNNdistplot(x, k = 5)

## ----knndistplot-multiple-----------------------------------------------------
kNNdistplot(x, k = c(1, 5, 10))
legend("topleft", legend = c("k = 1", "k = 5", "k = 10"),
       col = 1:3, lty = 1, bty = "n")

## ----knndistplot-minpts-------------------------------------------------------
kNNdistplot(x, minPts = 6)

## ----components-knn-----------------------------------------------------------
comp_directed <- comps(knn3, mutual = FALSE)
comp_mutual <- comps(knn3, mutual = TRUE)

c(
  components_any_direction = length(unique(comp_directed)),
  components_mutual = length(unique(comp_mutual))
)

## ----components-frnn----------------------------------------------------------
fr_comp <- comps(fr)
table(fr_comp)

plot(x, col = fr_comp, pch = 19, asp = 1,
     main = "Components of the fixed-radius graph")

## ----components-snn-----------------------------------------------------------
snn_comp <- comps(snn5)
table(snn_comp)

## ----components-dist----------------------------------------------------------
comp_dist <- comps(dist(x), eps = 0.25)
comp_fr <- comps(fr)

# Component numbers are arbitrary; compare which pairs share a component.
all(outer(comp_dist, comp_dist, `==`) ==
      outer(comp_fr, comp_fr, `==`))

## ----sorting------------------------------------------------------------------
fr_unsorted <- frNN(x, eps = 0.25, sort = FALSE)
fr_sorted <- sort(fr_unsorted)

data.frame(
  id = fr_sorted$id[[1]],
  distance = fr_sorted$dist[[1]]
)

## ----other-distance-----------------------------------------------------------
d_manhattan <- dist(x, method = "manhattan")

knn_manhattan <- kNN(d_manhattan, k = 5)
fr_manhattan <- frNN(d_manhattan, eps = 0.25)
snn_manhattan <- sNN(d_manhattan, k = 10)

c(
  knn_metric = knn_manhattan$metric,
  frnn_metric = fr_manhattan$metric,
  snn_metric = snn_manhattan$metric
)

## ----search-strategies--------------------------------------------------------
knn_tree <- kNN(x, k = 5, search = "kdtree")
knn_linear <- kNN(x, k = 5, search = "linear")
knn_dist <- kNN(x, k = 5, search = "dist")

c(
  tree_vs_linear = isTRUE(all.equal(knn_tree$dist, knn_linear$dist)),
  tree_vs_dist = isTRUE(all.equal(knn_tree$dist, knn_dist$dist))
)

