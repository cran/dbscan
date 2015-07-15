\name{dbscan}
\alias{dbscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  DBSCAN
}
\description{
  Fast reimplementation of DBSCAN using a kd-tree.
}
\usage{
  dbscan(x, eps, minPts = 5, bucketSize = 10, splitRule = "suggest", approx = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data matrix}
  \item{eps}{ size of the epsilon neighborhood.}
  \item{minPts}{ number of minimum points (for core points).}
  \item{bucketSize}{ max size of the kd-tree leafs. }
  \item{splitRule}{ rule to split the tree. One of "STD" (kd),
    "MIDPT", "FAIR", "MIDPT", "SL_FAIR" or "SUGGEST". }
  \item{approx}{ use approximate nearest neighbors. All NN up to a distance of
    a factor of 1+\code{approx} eps may be used. Some actual NN may be
    omitted leading to spurious clusters and noise points.
    However, the algorithm will enjoy a significant speedup. }
}
%\details{
  %%  ~~ If necessary, more details than the description above ~~
    %}
\value{
  A integer vector with cluster assignments. Zero indicates noise points.
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise. Institute for Computer Science, University of Munich. Proceedings of 2nd International Conference on Knowledge Discovery and Data Mining (KDD-96).
}
%\author{
  %%  ~~who you are~~
    %}
%\note{
  %%  ~~further notes~~
    %}

%% ~Make other sections like Warning with \section{Warning }{....} ~

  %\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
      %}
\examples{
data(iris)
iris <- as.matrix(iris[,1:4])

res <- dbscan(iris, .4, 4)
pairs(iris, col=res+1)

## compare with dbscan from package fpc
res2 <- fpc::dbscan(iris, .4, 4)
res2 <- res2$cluster
pairs(iris, col=res2+1)

## make sure both version produce the same results
all(res == res2)

## find suitable eps parameter (look at knee)
kNNdistplot(iris, k=4)


## example data from fpc
set.seed(665544)
n <- 600
x <- cbind(runif(10, 0, 10)+rnorm(n, sd=0.2), runif(10, 0, 10) + rnorm(n,
  sd=0.2))

res <- dbscan::dbscan(x, .2, 4)
plot(x, col=res+1)

## compare speed against fpc version
t_dbscan <- microbenchmark::microbenchmark(
  dbscan::dbscan(x, .2, 4), times = 10, unit="ms")
t_fpc <- microbenchmark::microbenchmark(
  fpc::dbscan(x, .2, 4), times = 10, unit="ms")
boxplot(rbind(t_dbscan, t_fpc))

## speedup
median(t_fpc$time)/median(t_dbscan$time)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

