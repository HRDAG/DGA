\name{remove.close}
\alias{remove.close}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Helper Function to Tell Which Points Are Near the Boundary of a Circle}
\description{Used in venn3 to tell which of the potential points to be plotted are near the boundary of the circle defned by x, y, and r.}
\usage{
remove.close(ps, x,y,r)}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ps}{an n x 2 matrix of potential points.}
  	\item{x}{ the x coordinate of the center of the circle.}
  	\item{y}{ the y coordinate of the center of the circle.}
  	\item{r}{ the radius of the circle}
}
\value{
\item{inds}{tells which points are too close to the edge of the circle. }
}

\author{
Kristian Lum \email{kl@hrdag.org}
}

\examples{


ps <- cbind(runif(100), runif(100))
inds <- remove.close(ps, .5, .5, .1)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{circle }
