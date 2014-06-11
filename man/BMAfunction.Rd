\name{BMAfunction}
\alias{BMAfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BMAfunction(Y, Nmissing, delta, graphs, logprior = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{Nmissing}{
%%     ~~Describe \code{Nmissing} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{graphs}{
%%     ~~Describe \code{graphs} here~~
}
  \item{logprior}{
%%     ~~Describe \code{logprior} here~~
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
function (Y, Nmissing, delta, graphs, logprior = NULL) 
{
    modNweights <- matrix(nrow = length(graphs), ncol = length(Nmissing))
    p <- length(dim(Y))
    compMat <- MakeCompMatrix(p, delta, Y)
    j <- 1
    for (graph in graphs) {
        binC <- t(sapply(graph$C, tmpfun, p = p))
        decC <- apply(t(binC) * rev(2^(0:(p - 1))), 2, sum)
        compMats <- compMat[decC, ]
        if (!is.null(nrow(compMats))) {
            cliqueML <- apply(compMats, 2, sum)
        }
        else {
            cliqueML <- compMats
        }
        if (!is.null(graph$S)) {
            binS <- t(sapply(graph$S, tmpfun, p = p))
            decS <- apply(t(binS) * rev(2^(0:(p - 1))), 2, sum)
            compMats <- compMat[decS, ]
            if (!is.null(nrow(compMats))) {
                sepML <- apply(compMats, 2, sum)
            }
            else {
                sepML <- compMats
            }
        }
        else {
            sepML <- 0
            decS <- NULL
        }
        nsubgraphs <- length(decC) - length(decS)
        alpha <- rep(delta, length(Y))
        modNweights[j, ] <- (cliqueML - sepML + nsubgraphs * 
            (lgamma(sum(alpha)) - lgamma(Nmissing + sum(Y) + 
                sum(alpha))))
        j <- j + 1
    }
    multicoef <- lgamma(Nmissing + sum(Y) + 1) - sum(lgamma(Y[-1] + 
        1)) - lgamma(Nmissing + 1)
    modNweights <- t(t(modNweights) + multicoef)
    if (is.null(logprior)) {
        logprior <- -log(sum(Y) + Nmissing)
    }
    modNweights <- t(t(modNweights) + logprior)
    modNweights <- modNweights - max(modNweights)
    weights <- exp(modNweights)
    weights <- weights/sum(weights)
    return(weights)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
