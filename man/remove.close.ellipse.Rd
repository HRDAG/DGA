\name{remove.close.ellipse}
\alias{remove.close.ellipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Helper Function to Tell Which Points are Near the Boundary of the Ellipse}
\description{
A helper function.}
\usage{
remove.close.ellipse(ps, x,y,a,b,alpha)}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{ps}{an n x 2 matrix of potential points.}
  	\item{x}{ the x coordinate of the center of the ellipse.}
  	\item{y}{ the y coordinate of the center of the ellipse.}
  	\item{a}{ the x-radius of the ellipse.}
  	\item{b}{the y-radius of the ellipse.}
  	\item{alpha}{the angle of rotation of the ellipse.}
}

\value{
\item{inds}{a vector of length nrow(ps) that tells whether each row of ps is near the border of the ellipse defined by x,y,a,b, and alpha. }
}

\author{
Kristian Lum \email{kl@hrdag.org}}

\examples{

## The function is currently defined as
ps <- cbind(runif(100), runif(100))
inds <- remove.close.ellipse(ps, .5, .5, .1, .3, 1)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ellipse}
