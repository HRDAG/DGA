\name{MakeCompMatrix}
\alias{MakeCompMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Component-wise Matrix of Log Marginal Likelihoods}
\description{
Calls CompLogML to create a matrix of number of possible components by length(Nmissing) log marginal likelihoods. Calculates the log marginal likehood of each possible marginal table for every value of Nmissing. }
\usage{
MakeCompMatrix(p, delta, Y, Nmissing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Number of lists}
  \item{delta}{
Prior hyperparameter of the Dirichlet distribution.}
  \item{Y}{The \code{2^k} matrix of list intersection counts.}
  \item{Nmissing}{
  The vector of possible values for the missing cell. }
}

\value{
A matrix of log marginal likelihoods.
}

\author{
James Johndrow \email{james.johndrow@gmail.com} and Kristian Lum \email{kl@hrdag.org}
}

\examples{

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
\keyword{ Bayesian model averaging}
\keyword{ marginal likelihood }% __ONLY ONE__ keyword per line
