\name{ellipse.ind}
\alias{ellipse.ind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Helper Function Used by Venn4}
\description{
Takes potential points to be plotted in the venn diagrams and returns whether the point is inside or outside of the ellipse described by x, y, a, b, and alpha.}
\usage{
ellipse.ind(ps, x,y, a, b, alpha)}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ps}{a n x 2 matrix of coordinates.}
  	\item{x}{ the x coordinate of the center of the ellipse.}
  	\item{y}{ the y coordinate of the center of the ellipse.}
  	\item{a}{ the x-radius of the ellipse.}
  	\item{b}{ the y-radius of the ellipse.}
  	\item{alpha}{the angle of rotation of the ellipse}
}

\value{
a length n vector indicating whether each point is inside the ellipse. 
}
\author{
Kristian Lum \email{kl@hrdag.org}
}

\examples{


## The function is currently defined as
ps <- cbind(runif(100), runif(100))
plot(ellipse(0,0,.5, .3, 0), type = 'l')
inds <- ellipse.ind(ps, 0, 0, .5, .3, 0)
points(inds)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ellipse}
