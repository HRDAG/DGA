\name{check.strata}
\alias{check.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks Each Stratum for Suitability for Capture-Recapture}
\description{Takes in list intersection counts and source list totals as produced by make.strata. It then checks whether there are between three and five lists, whether all of the lists are non-empty, and whether all of the lists overlap with some other list. }

\usage{check.strata(strata)}
\arguments{
	\item{strata}{A list of list overlaps and source countsin the format of the output of make.strata. list.overlaps contains a data frame of list overlaps by stratum. source.counts contains the number of records by source and stratum.}
}

\value{A boolean indicating whether any serious problems have been found with the strata.}

\author{
Kristian Lum \email{kl@hrdag.org}
}
\note{
This does not issue a warning for cases where some subset of lists is not connected to the others, e.g. Lists A and B have overlap with each other, lists C and D have overlap with each other, but no records from A or B overlap with lists C or D. We suggest that you examine the list intersection counts manually as well.}


\examples{

library(chron)

N <- 1000
overlaps <- data.frame(l1=rbinom(N, 1, .5), l2=rbinom(N, 1, .5), l3=rbinom(N, 1, .5))
dates <- paste(rep(2015, N), "-", sample(1:12, N, replace = TRUE), "-", 
	sample(1:28, N, replace = TRUE))
dates <- chron(dates, format=c(dates="y-m-d"))
locations <- sample(c("A", "B", "C", "D"), N, replace = TRUE)

# Aggregate only by week: 
strata <- make.strata(overlaps, dates, date.def = "weekly")
check <- check.strata(strata)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{list overlaps}
