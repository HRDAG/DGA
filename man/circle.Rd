\name{circle}
\alias{circle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Helper Function Used in venn3}
\description{
Takes the parameters of a circle and returns points on its perimeter to be plotted to make circles for a venn diagram.}
\usage{
circle(x,y,r)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the x coordinate of the center of the circle.}
  \item{y}{the y coordinate of the center of the circle.}
  \item{r}{the radius of the circle}
}

\value{
\item{inds}{the x,y coordinates of the periphery of a circle, to be used in venn3. }
}

\author{
Kristian Lum \email{kl@hrdag.org}
}

\examples{

plot(circle(0,0,1), type = 'l')
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{circle }
