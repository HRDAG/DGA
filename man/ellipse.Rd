\name{ellipse}
\alias{ellipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Helper Function Used by Venn4 to Define the Perimeter of an Ellipse}
\description{
Draws the ellipses used in venn4.}
\usage{
ellipse(x,y,a,b,alpha)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the x coordinate of the center of the ellipse.}
  \item{y}{ the y coordinate of the center of the ellipse.}
  \item{a}{ the x-direction radius.}
  \item{b}{the y-direction radius.}
  \item{alpha}{the angle of rotation of the ellipse}
}

\value{
points that define the perimeter of an ellipse. 
}

\author{
Kristian Lum \email{kl@hrdag.org}
}

\examples{

plot(ellipse(0,0,.5, .2, 1))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ellipse}
