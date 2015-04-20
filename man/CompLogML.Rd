\name{CompLogML}
\alias{CompLogML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes Marginal Likelihoods for Each Clique and Value of Nmissing}
\description{
 Assembles all of the pieces of the marginal likelihoods to be used to calculate the posterior probability of each model/value of Nmissing.}
\usage{
CompLogML(D, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{A marginal table of the list overlap counts.}
  \item{delta}{The prior hyper parameter for the Dirichlet distribution. This has been properly re-scaled in bma.cr so that all marginal tables have consistent priors.}
}

\value{
The log marginal likelihood of the marginal table.
}
\references{
Madigan, David, and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population."  Biometrika 84.1 (1997): 19-31.}
\author{
James Johndrow \email{james.johndrow@gmail.com} and Kristian Lum \email{kl@hrdag.org}
}

\examples{


## The function is currently defined as
function (D, delta) 
{
    out <- apply(lgamma(D + delta), 1, sum) - apply(lgamma(D * 
        0 + delta), 1, sum)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{capture-recapture }
\keyword{multiple systems estimation}% __ONLY ONE__ keyword per line
\keyword{BMA }
