\name{kNNdistplot}
\alias{kNNdistplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the k-Nearest Neighbor Distance
}
\description{
This plot can be used to help find a suitable value for the
\code{eps} neighborhood for DBSCAN. Look for the knee in the plot.
}
\usage{
kNNdistplot(x, k = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the data set as a matrix. }
  \item{k}{ number of nearest neighbors used (use minPoints). }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
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

kNNdistplot(iris, k=4)
## the knee is around a distance of .5

cl <- dbscan(iris, eps = .4, minPts = 4)
pairs(iris, col = cl+1L)
## Note: black are noise points
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
