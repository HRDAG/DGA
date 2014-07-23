\name{plotPosteriorN}
\alias{plotPosteriorN}
\title{
A function to visualize your results.}
\description{b
This function plots the model averaged posterior distribution of the total number of elements (the  solid line) and the contribution to the posteriof of each of the models (dotted lines)}
\usage{
plotPosteriorN(weights, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weights}{
The output of BMAfunction.}
  \item{N}{
N + Nmissing. Or, if you prefer, just Nmissing. The former shows the posterior distribution of the total population size; the latter shows the posterior distribution of the number of missing elements.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot.
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
##### 5 list example from M & Y #######

delta <- .5
Y = c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
Y <- array(Y, dim=c(2,2,2,2,2))
Nmissing <- 1:300
N <- Nmissing + sum(Y) 
data(graphs5)
weights <- BMAfunction(Y,  Nmissing, delta, graphs)
plotPosteriorN(weights, N)


##### 3 list example from M & Y #######
Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
Y <- array(Y, dim=c(2,2,2))

delta <- 1
a <- 13.14
b <- 55.17


Nmissing <- 1:300
N <- Nmissing + sum(Y)

logprior <- N*log(b) - (N + a)*log(1 + b)  + lgamma(N + a) - lgamma(N + 1) - lgamma(a)

data(graphs3)
weights <- BMAfunction(Y,  Nmissing, delta, graphs, logprior)
plotPosteriorN(weights, N)


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
