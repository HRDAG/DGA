\name{dga-package}
\alias{dga-package}
\alias{dga}
\docType{package}
\title{
Bayesian model averaging for multiple systems estimation
}
\description{
This package performs Bayesian model averaging over decomposable graphical representations of list dependence with the goal of estimating the number of uncounted elements from the universe of elements that could have been recorded or ``captured".  
}
\details{
\tabular{ll}{
Package: \tab dga\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-11\cr
License: \tab What license is it under?\cr
}
To use this package, you really only need to use BMAfunction. We should really rename that. }
\author{
James Johndrow (james.johndrow@gmail.com) and Kristian Lum (kristianlum@gmail.com)

Maintainer: Patrick Ball (pball@hrdag.org)
}
\references{
Madigan, David, and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population."  Biometrika 84.1 (1997): 19-31.}


\keyword{ capture-recapture }
\keyword{ multiple systems estimation }
\keyword{ Bayesian model averaging }


\seealso{

}
\examples{
#### 5 list example from M & Y ##########
delta <- .5
Y = c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
Y <- array(Y, dim=c(2,2,2,2,2))
Nmissing <- 1:300
N <- Nmissing + sum(Y)
#weights <- BMAfunction(Y,  Nmissing, delta)
#plotPosteriorN(weights, N)
 
##### 3 list example from M & Y #######
Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
Y <- array(Y, dim=c(2,2,2))

delta <- 1
a <- 13.14
b <- 55.17


Nmissing <- 1:300
N <- Nmissing + sum(Y)

logprior <- N*log(b) - (N + a)*log(1 + b)  + lgamma(N + a) - lgamma(N + 1) - lgamma(a)}

#weights <- BMAfunction(Y,  Nmissing, delta, logprior)
#plotPosteriorN(weights, N)