## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(dbscan)

## ----data---------------------------------------------------------------------
set.seed(4)

regular <- rbind(
  cbind(rnorm(150, -1, 0.30), rnorm(150, 0, 0.30)),
  cbind(rnorm(150,  2, 0.65), rnorm(150, 0, 0.65))
)
outliers <- rbind(
  c(-3.0,  2.4), c( 0.0,  2.9), c(4.5,  2.7), c(5.0,  0.0),
  c(-3.1, -2.3), c(-0.1, -2.8), c(2.3, -3.1), c(4.6, -2.4)
)
x <- rbind(regular, outliers)
known_outlier <- seq_len(nrow(x)) > nrow(regular)

plot(
  x,
  pch = ifelse(known_outlier, 4, 19),
  col = ifelse(known_outlier, "red3", "grey35"),
  asp = 1,
  xlab = "x1", ylab = "x2",
  main = "Example data"
)

## ----helper-------------------------------------------------------------------
plot_scores <- function(x, score, main, n = 8) {
  size <- 0.5 + 2.5 * (score - min(score)) / diff(range(score))
  top <- order(score, decreasing = TRUE)[seq_len(n)]
  plot(x, pch = 19, col = "grey65", asp = 1, main = main,
       xlab = "x1", ylab = "x2")
  points(x, pch = 1, col = "red3", cex = size, lwd = 1.5)
  text(x[top, , drop = FALSE], labels = top, pos = 3, cex = 0.7)
  invisible(top)
}

## ----point-density------------------------------------------------------------
frequency <- pointdensity(x, eps = 0.45, type = "frequency")
summary(frequency)

# Reverse the sign so that larger values consistently mean more unusual.
density_score <- -frequency
plot_scores(x, density_score, "Low fixed-radius density")

## ----density-scale------------------------------------------------------------
d9 <- sort(kNNdist(x, k = 9))
plot(
  d9,
  type = "l",
  xlab = "Observations sorted by 9-NN distance",
  ylab = "9-NN distance",
  main = "Neighborhood distances"
)
abline(h = 0.45, col = "red3", lty = 2)

## ----lof----------------------------------------------------------------------
lof_score <- lof(x, minPts = 10)
summary(lof_score)
plot_scores(x, lof_score, "Local Outlier Factor (minPts = 10)")

## ----lof-sensitivity----------------------------------------------------------
minpts <- c(5, 10, 20, 40)
lof_scores <- sapply(minpts, function(m) lof(x, minPts = m))
colnames(lof_scores) <- paste0("minPts_", minpts)

# How many of the top 12 from minPts = 10 remain in each top-12 list?
reference <- order(lof_scores[, "minPts_10"], decreasing = TRUE)[1:12]
data.frame(
  minPts = minpts,
  overlap = apply(lof_scores, 2, function(s)
    length(intersect(reference, order(s, decreasing = TRUE)[1:12])))
)

## ----glosh--------------------------------------------------------------------
glosh_score <- glosh(x, k = 10)
summary(glosh_score)
plot_scores(x, glosh_score, "GLOSH (k = 10)")

## ----hdbscan------------------------------------------------------------------
hdb <- hdbscan(x, minPts = 10)
all.equal(hdb$outlier_scores, glosh_score)

head(data.frame(
  cluster = hdb$cluster,
  membership = hdb$membership_prob,
  outlier_score = hdb$outlier_scores
))

## ----glosh-hclust-------------------------------------------------------------
hc <- hclust(dist(x), method = "single")
glosh_from_hierarchy <- glosh(hc, k = 10)
summary(glosh_from_hierarchy)

## ----candidates---------------------------------------------------------------
top_n <- function(score, n)
  order(score, decreasing = TRUE)[seq_len(n)]

candidates <- data.frame(
  pointdensity = top_n(density_score, 8),
  LOF = top_n(lof_score, 8),
  GLOSH = top_n(glosh_score, 8)
)
candidates

# Evaluation is possible because this simulated example has labels.
sapply(candidates, function(i) sum(known_outlier[i]))

## ----dbscan-noise-------------------------------------------------------------
db <- dbscan(x, eps = 0.45, minPts = 10)
table(noise = db$cluster == 0, known_outlier)

plot(
  x,
  col = db$cluster + 1L,
  pch = ifelse(db$cluster == 0, 4, 19),
  asp = 1,
  xlab = "x1", ylab = "x2",
  main = "DBSCAN clusters and noise"
)

