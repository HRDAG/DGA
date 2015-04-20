\name{venn4}
\alias{venn4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Four List Venn Diagram
}
\description{
A function that plots a venn diagram of 4 lists. One point is plotted in each region for each record that falls into the corresponding list overlap.}
\usage{
venn4(overlap.counts, main = NULL, num.test.points = 100000, 
	p.cex = .75, cex.main = 1)  
}
\arguments{
	\item{overlap.counts}{A vector of length \code{2^4} that gives the number of records in each overlap in lexicographic order, i.e. 0000, 0001, 0010, 0011, 0100, etc.}
  	\item{main}{ the title of the graph}
  	\item{num.test.points}{ how many test points to generate as potentials to be plotted in the circles. }
  	\item{p.cex}{ the size of the points to be plotted}
  	\item{cex.main}{the size of the title}

}

\value{A venn diagram of the list overlap structure for four lists. Each region of the plot contains points representing each record in that list intersection. }

\author{
Kristian Lum \email{kl@hrdag.org}
}


\examples{

overlap.counts <- rpois(16, 50)
venn4(overlap.counts, main = "example diagram")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{venn diagram}
