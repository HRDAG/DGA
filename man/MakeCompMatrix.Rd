\name{MakeCompMatrix}
\alias{MakeCompMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeCompMatrix(p, delta, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (p, delta, Y) 
{
    compLMLs <- matrix(0, nrow = 2^p - 1, ncol = length(Nmissing))
    bins <- integer.base.b(1:(2^p - 1), 2)
    for (i in 1:(2^p - 1)) {
        inds <- which(bins[i, ] == 1)
        D <- c(apply(Y, inds, sum))
        Dmat <- t(matrix(D, ncol = length(Nmissing), nrow = length(D)))
        Dmat[, 1] <- Dmat[, 1] + Nmissing
        alpha <- rep(delta * 2^(p - sum(bins[i, ])), ncol(Dmat))
        compLMLs[i, ] <- CompLogML(Dmat, alpha)
    }
    return(compLMLs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
