\name{circle.ind}
\alias{circle.ind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Helper Function for venn3}
\description{Used in venn3 to tell whether proposed points are inside of the given circle.}
\usage{
circle.ind(ps, x,y, r)}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ps}{a n x 2 matrix of coordinates.}
  	\item{x}{the x coordinate of the center of the circle.}
  	\item{y}{the y coordinate of the center of the circle.}
  	\item{r}{the radius of the circle.}
  
}

\value{
a length n vector telling whether each row of ps is inside the given circle. }

\author{
Kristian Lum \email{kl@hrdag.org}
}

\examples{


ps <- cbind(runif(100), runif(100))
plot(circle(0,0,1), type = 'l')
inds <- circle.ind(ps, 0, 0, 1)
points(inds)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{circle}
