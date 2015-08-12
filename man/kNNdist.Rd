\name{kNNdist}
\alias{kNNdist}
\alias{kNNdistplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate and plot the k-Nearest Neighbor Distance
}
\description{
Fast caclulation of the k-nearest neighbor distances in a matrix of points.
The plot can be used to help find a suitable value for the
\code{eps} neighborhood for DBSCAN. Look for the knee in the plot.
}
\usage{
kNNdist(x, k, search = "kdtree", bucketSize = 10, splitRule = "suggest",
  approx = 0)
kNNdistplot(x, k = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the data set as a matrix. }
  \item{k}{ number of nearest neighbors used (use minPoints). }
  \item{search}{ nearest neighbor search strategy (one of "kdtree"
    or "linear").}
  \item{bucketSize}{ max size of the kd-tree leafs. }
  \item{splitRule}{ rule to split the tree. One of "STD" (kd),
    "MIDPT", "FAIR", "MIDPT", "SL_FAIR" or "SUGGEST". }
  \item{approx}{ use approximate nearest neighbors. All NN up to a distance of
    a factor of 1+\code{approx} eps may be used. Some actual NN may be
    omitted leading to spurious clusters and noise points.
    However, the algorithm will enjoy a significant speedup. }
  \item{...}{further arguments are passed on to \code{kNNdist}.}
  }
\details{
See \code{\link{kNN}} for a discusion of the kd-tree related parameters.
}
\value{
\code{kNNdist} returns a numeric vector with the distance to its k nearest
neighbor.
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{kNN}}.
}% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(iris)
iris <- as.matrix(iris[,1:4])

kNNdist(iris, k=4, search="kd")
kNNdistplot(iris, k=4)
## the knee is around a distance of .5

cl <- dbscan(iris, eps = .5, minPts = 4)
pairs(iris, col = cl$cluster+1L)
## Note: black are noise points
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model }
\keyword{ plot }
