
library("dbscan")
library("testthat")


context("LOF")

set.seed(665544)
n <- 100
x <- cbind(
  x=runif(10, 0, 5) + rnorm(n, sd=0.4),
  y=runif(10, 0, 5) + rnorm(n, sd=0.4)
)

### calculate LOF score
lof <- lof(x, k=4)

expect_identical(length(lof), nrow(x))

## FIXME: more tests
