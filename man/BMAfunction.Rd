\name{BMAfunction}
\alias{BMAfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that performs Bayesian model averaging for capture-recapture
}
\description{
This function averages over all decomposable graphical models for $p$ lists. }
\usage{
BMAfunction(Y, Nmissing, delta, graphs, logprior = NULL)
}
\arguments{
  \item{Y}{
\code{Y} are the list intersection counts, arranged in a $2^p$ array. Let $i = i_1 i_2 … i_p$ represent a list intersection, where $i_j = 2$ indicates inclusion on list $j$ and $i_j = 1$ indicates exclusion from list $j$. The $i$th cell of the array is then the number of individuals that fall into list intersection $i$. For example, if $i_1 = 1$, $i_2 = 2$, and $i_3 = 2$ for $p=3$ lists, then $Y_{121}$ contains the count of the number of individuals that did not appear on list 1 and did appear on lists 2 and 3. Although estimating $Y_{00…0}$ is the goal of this code, the $00…0$th cell should be set to 0.  
}
  \item{Nmissing}{
A vector of all possible values for the number of individuals that appear on no list. 

}
  \item{delta}{
The hyper-parameter for the Dirichlet distribution. A smaller value indicates less prior information.}
  \item{graphs}{A pre-computed list of all decomposable graphical models for p lists. Need to run data(graphsp), where p is the number of lists, and use this as an input. See example.}
  
  \item{logprior}{
The log of the prior probability of each value in Nmissing. If left blank, this will default to the -log(Nmissing).}
}
\details{
This is the main function in this package that calls the other functions listed. This is probably the one you want to use.}
\value{
This function returns a matrix of weights, where rows correspond to models and columns correspond to values of Nmissing. Thus, the $ij$th entry of the output matrix is the posterior probability of the $i$th model and the $j$th entry of $Nmissing$. Row sums return posterior probabilities by model. Column sums return posterior probabilities by value of  Nmissing.}
\references{
Madigan, David, and Jeremy C. York. "Bayesian methods for estimation of the size of a closed population."  Biometrika 84.1 (1997): 19-31.}
\author{
James Johndrow (james.johndrow@gmail.com) and Kristian Lum (kristianlum@gmail.com)}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
#### 5 list example from M & Y ##########
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

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ capture-recapture }
\keyword{ multiple systems estimation }% __ONLY ONE__ keyword per line
\keyword{ BMA }
