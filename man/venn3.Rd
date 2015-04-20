\name{venn3}
\alias{venn3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Three List Venn Diagram}
\description{
A function that plots a venn diagram of 3 lists. One point is plotted in each region for each record that falls into the corresponding list overlap.}
\usage{
venn3(overlap.counts, main = NULL, num.test.points = 100000, p.cex = .75, 
                  write_numbers = FALSE, t.cex = 1.25, cex.main = 1)  }
\arguments{
\item{overlap.counts}{A vector of length \code{2^3} that gives the number of records in each overlap in lexicographic order, i.e. 001, 010, 011, 100, etc.}
  \item{main}{ the title of the graph}
  \item{num.test.points}{ how many test points to generate as potentials to be plotted in the circles. }
  \item{p.cex}{ the size of the points to be plotted}
  \item{write_numbers}{ indicates whether to print the number of points in each region.}
  \item{t.cex}{the size of the text to write the numbers.}
  \item{cex.main}{the size of the title}
}

\value{a 3-way venn diagram with points inside of each segment representing the number of records on each list overlap.}


\author{
Kristian Lum \email{kl@hrdag.org}}

\examples{

overlap.counts <- rpois(8, 30)
venn3(overlap.counts, main = "example diagram")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{venn diagram}
