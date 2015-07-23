\name{dbscan}
\alias{dbscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  DBSCAN
}
\description{
  Fast reimplementation of DBSCAN using a kd-tree. The implementation is
  significantly faster and can work with larger data sets then dbscan in
  \pkg{fpc}.
}
\usage{
  dbscan(x, eps, minPts = 5, borderPoints = TRUE,
    search = "kdtree", bucketSize = 10,
    splitRule = "suggest", approx = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data matrix.}
  \item{eps}{ size of the epsilon neighborhood.}
  \item{minPts}{ number of minimum points in the eps region (for core points).
      Default is 5 points.}
  \item{borderPoints}{ logical; should border points be assigned. If
    \code{FALSE} then border points are considered noise
    (see DBSCAN* in Campello et al, 2013).}
  \item{search}{ nearest neighbor search strategy (one of "kdtree"
    or "linear").}
  \item{bucketSize}{ max size of the kd-tree leafs. }
  \item{splitRule}{ rule to split the tree. One of "STD" (kd),
    "MIDPT", "FAIR", "MIDPT", "SL_FAIR" or "SUGGEST". "SUGGEST" uses ANNs best
    guess.}
  \item{approx}{ relative error bound for approximate nearest neighbor searching.}
}
\details{
This implementation of DBSCAN implements the original algorithm as described by
Ester et al (1996). Linear nearest neighbor search is implemented. To speed up nearest neighbor search the kd-tree data structure implemented in the ANN library (see Mount and Arya, 2010) is used, where the fixed-radius is slightly modified to return all and not just k nearest neighbors.

\code{bucketSize} and \code{splitRule} influence how the kd-tree is built.
\code{approx} uses the approximate nearest neighbor search implemented in ANN.
All nearest neighbors up to a distance of \code{eps}/(1+\code{approx})
will be considered and all with a distance greater than \code{eps} will not
be considered. The other points might be considered. Note that this results in
some actual nearest neighbors being omitted leading to spurious clusters and noise points. However, the algorithm will enjoy a significant speedup. For more details see Mount and Arya (2010).

Border points are arbitrarily assigned to clusters in the original algorithm.
DBSCAN* (see Campello et al 2013) treats all border points as noise points. This is implemented with \code{borderPoints = FALSE}.

}
\value{
  A integer vector with cluster assignments. Zero indicates noise points.
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise. Institute for Computer Science, University of Munich. \emph{Proceedings of 2nd International Conference on Knowledge Discovery and Data Mining (KDD-96).}

Campello, R. J. G. B.; Moulavi, D.; Sander, J. (2013). Density-Based Clustering Based on Hierarchical Density Estimates. \emph{Proceedings of the 17th Pacific-Asia Conference on Knowledge Discovery in Databases, PAKDD 2013.} Lecture Notes in Computer Science 7819. p. 160.

David M. Mount and Sunil Arya (2010). ANN: A Library for Approximate Nearest Neighbor Searching, \url{https://www.cs.umd.edu/~mount/ANN/}.
}
%\author{
  %%  ~~who you are~~
    %}
%\note{
  %%  ~~further notes~~
    %}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fpc]{dbscan}} in \pkg{fpc}.
}
\examples{
data(iris)
iris <- as.matrix(iris[,1:4])

res <- dbscan(iris, .4, 4)
pairs(iris, col=res+1)

## compare with dbscan from package fpc (only if installed)
if (requireNamespace("fpc", quietly = TRUE)) {
  res2 <- fpc::dbscan(iris, .4, 4)
  res2 <- res2$cluster
  pairs(iris, col=res2+1)

  ## make sure both version produce the same results
  all(res == res2)
}

## find suitable eps parameter (look at knee)
kNNdistplot(iris, k=4)


## example data from fpc
set.seed(665544)
n <- 600
x <- cbind(runif(10, 0, 10)+rnorm(n, sd=0.2), runif(10, 0, 10) + rnorm(n,
  sd=0.2))

res <- dbscan::dbscan(x, .2, 4)
plot(x, col=res+1)

## compare speed against fpc version (if microbenchmark is installed)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  t_dbscan <- microbenchmark::microbenchmark(
    dbscan::dbscan(x, .2, 4), times = 10, unit="ms")
  t_dbscan_linear <- microbenchmark::microbenchmark(
    dbscan::dbscan(x, .2, 4, search="linear"), times = 10, unit="ms")
  t_fpc <- microbenchmark::microbenchmark(
    fpc::dbscan(x, .2, 4), times = 10, unit="ms")

  boxplot(rbind(t_fpc, t_dbscan_linear, t_dbscan),
    names=c("fpc (R)", "dbscan (linear)", "dbscan (kdtree)"),
    main = "Runtime comparison in ms")

  ## speedup of the kd-tree-based version compared to the fpc implementation
  median(t_fpc$time)/median(t_dbscan$time)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model }
\keyword{ clustering }

