\name{CompLogML}
\alias{CompLogML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A helper function for BMAfunction}
\description{
 This function computes the log marginal likelihood of each fully connected sub-graph of the $p$ lists.}
\usage{
CompLogML(D, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
A marginal table of \code{Y}}
  \item{delta}{
The prior hyper parameter for  the Dirichlet distribution.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The log marginal likelihood of the marginal table.
}
\references{
Madigan, David, and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population."  Biometrika 84.1 (1997): 19-31.}
\author{
James Johndrow (james.johndrow@gmail.com) and Kristian Lum (kristianlum@gmail.com)}

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
\keyword{ multiple systems estimation}% __ONLY ONE__ keyword per line
\keyword{ BMA }
