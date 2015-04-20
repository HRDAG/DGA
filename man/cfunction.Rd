\name{cfunction}
\alias{cfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Helper Function for make.strata}
\description{
A helper function used in make.strata to make list overlap counts.}
\usage{
cfunction(x, nlist)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{capture histories, transformed from binary to decimal}
  \item{nlist}{ the number of lists}
}

\value{
a table of the number of records with each capture history 
}

\author{
Kristian Lum \email{kl@hrdag.org}}

\examples{

## The function is currently defined as
cfunction <- function(x, nlist){
  out <- table(c(x, 0:(2^nlist-1)))-1
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stratification}
