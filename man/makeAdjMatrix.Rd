\name{makeAdjMatrix}
\alias{makeAdjMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjacency Matrix Converter}
\description{
Converts a cliques and separators into an adjacency matrix representation of a graph.}
\usage{
makeAdjMatrix(graph, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
All the graphs.
}
  \item{p}{
The number of lists.}
}
\details{
This is only a helper function to use in the experimental plotting function PlotTopGraphs.}
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
function (graph, p) 
{
    Adj <- matrix(0, nrow = p, ncol = p)
    diag(Adj) <- 1
    for (i in 1:length(graph$C)) {
        if (length(graph$C[[i]]) > 1) {
            combns <- combn(graph$C[[i]], 2)
            Adj[combns[1], combns[2]] <- 1
        }
    }
    for (i in 1:length(graph$S)) {
        if (length(graph$S[[i]]) > 1) {
            combns <- combn(graph$S[[i]], 2)
            Adj[combns[1], combns[2]] <- 1
        }
    }
    Adj <- Adj + t(Adj)
    Adj[Adj > 1] <- 1
    return(Adj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
