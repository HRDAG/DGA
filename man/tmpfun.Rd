\name{tmpfun}
\alias{tmpfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Another Helper Function.}
\description{This makes a vector of zeroes and puts a 1 in the x'th place.}
\usage{
tmpfun(x, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{where the one should be.}
  \item{p}{the length of the vector.}
}

\value{
a vector of length p with a one in the xth location.
}

\author{
Kristian Lum \email{kl@hrdag.org}}

\examples{

## The function is currently defined as
function (x, p) 
{
    tmp <- rep(0, p)
    tmp[x] <- 1
    return((tmp))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{zero}

