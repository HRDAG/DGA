\name{plotTopModels}
\alias{plotTopModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to plot the top models.}
\description{
This plots the top graphical models. Still experimental.}
\usage{
plotTopModels(graphs, weights, p, how.many = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphs}{
The graphs used.}
  \item{weights}{
The output of BMAfunction.}
  \item{p}{
The number of lists.}
  \item{how.many}{
How many of the top models you want to plot.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot.}
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
function (graphs, weights, p, how.many = 3) 
{
    modelWeights <- apply(weights, 1, sum)
    or <- rev(order(modelWeights))
    par(mfrow = c(how.many, 1))
    for (i in 1:how.many) {
        graph <- graphs[[or[i]]]
        Adj <- makeAdjMatrix(graph, p)
        rownames(Adj) <- c("one", "two", "three")
        net <- as.network(Adj)
        plot(net, main = paste("posterior prob ", round(modelWeights[or[i]], 
            3)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
