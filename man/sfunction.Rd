\name{sfunction}
\alias{sfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Helper Function for make.strata.}
\description{This is the simplest function ever. It's just an apply to sum across columns. }
\usage{
sfunction(x)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ capture histories, as numbers}

}

\value{apply(x, 2, sum)}


\author{
Kristian Lum \email{kl@hrdag.org}}

\examples{


## The function is currently defined as
sfunction <- function(x){
  out <- apply(x, 2, sum)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stratification}
