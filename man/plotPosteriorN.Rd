\name{plotPosteriorN}
\alias{plotPosteriorN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPosteriorN(weights, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
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
function (weights, N) 
{
    plot(N, apply(weights, 2, sum), type = "l", col = "black", 
        lwd = 3, ylab = "Posterior Probability of N", xlab = "N", 
        ylim = c(0, 1.25 * max(apply(weights, 2, sum))))
    title("Posterior Summary")
    wts <- apply(weights, 1, sum)
    for (i in 1:nrow(weights)) {
        lines(Nmissing + sum(Y), weights[i, ], lwd = wts[i] * 
            3, lty = "dashed")
    }
    legend("topright", legend = c("Total Posterior Probability", 
        "One Model's Posterior Probability"), lty = c(1, 2), 
        cex = 0.75)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
