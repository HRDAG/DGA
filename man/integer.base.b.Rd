\name{integer.base.b}
\alias{integer.base.b}
Helper function 
\title{
Takes a decimal number and converts it to base b}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
integer.base.b(x, b = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A number.
}
  \item{b}{
The desired base.}
}
\details{
This was harvested from the internet here: https://stat.ethz.ch/pipermail/r-help/2003-September/038978.html. Posted by Spencer Graves. }
\value{
A number in base b.
}
\references{
https://stat.ethz.ch/pipermail/r-help/2003-September/038978.html}
\author{
Spencer Graves}
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
function (x, b = 2) 
{
    xi <- as.integer(x)
    if (any(is.na(xi) | ((x - xi) != 0))) 
        print(list(ERROR = "x not integer", x = x))
    N <- length(x)
    xMax <- max(x)
    ndigits <- (floor(logb(xMax, base = 2)) + 1)
    Base.b <- array(NA, dim = c(N, ndigits))
    for (i in 1:ndigits) {
        Base.b[, ndigits - i + 1] <- (x\%\%b)
        x <- (x\%/\%b)
    }
    if (N == 1) 
        Base.b[1, ]
    else Base.b
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binary}
\keyword{ decimal }% __ONLY ONE__ keyword per line
