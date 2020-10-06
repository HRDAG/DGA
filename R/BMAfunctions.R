#' Computes Marginal Likelihoods for Each Clique and Value of Nmissing
#'
#' Assembles all of the pieces of the marginal likelihoods to be used to
#' calculate the posterior probability of each model/value of Nmissing.
#'
#'
#' @param D A marginal array of the list overlap counts.
#' @param Nmissing The vector of possible values for the missing cell.
#' @param delta The prior hyper parameter for the Dirichlet distribution.
#'
#' @return The log marginal likelihood of the marginal table.
#' @author James Johndrow \email{james.johndrow@@gmail.com} and Kristian Lum
#' \email{kl@@hrdag.org}
#' @references Madigan, David, and Jeremy C. York. "Bayesian methods for
#' estimation of the size of a closed population."  Biometrika 84.1 (1997):
#' 19-31.
#' @keywords capture-recapture multiple systems estimation BMA
#'
#' @examples
#'
#' Y <- c(0, 27, 37, 19, 4, 4, 1, 1, 97, 22, 37, 25, 2, 1, 3, 5,
#'        83, 36, 34, 18, 3, 5, 0, 2, 30, 5, 23, 8, 0, 3, 0, 2)
#' Y <- array(Y, dim = c(2, 2, 2, 2, 2))
#'
#' # Compute marginal array over lists 1 and 3
#' D <- apply(Y, c(1, 3), sum)
#'
#' dga:::CompLogML(D, 1:300, 0.5)
CompLogML <- function(D, Nmissing, delta) {
  Nmissing <- Nmissing + D[1] + delta
  return(
    lgamma(Nmissing) +
      sum(lgamma(D[2:length(D)] + delta)) -
      length(D) * (lgamma(delta))
  )
}



#' Base Converter
#'
#' Takes a decimal number and converts it to base b.
#'
#' This was harvested from the internet here:
#' https://stat.ethz.ch/pipermail/r-help/2003-September/038978.html. Posted by
#' Spencer Graves.
#'
#' @param x A number.
#' @param b The desired base.
#' @return A number in base b.
#' @author Spencer Graves
#' @references https://stat.ethz.ch/pipermail/r-help/2003-September/038978.html
#' @keywords binary decimal
integer.base.b <- function(x, b = 2) {
  xi <- as.integer(x)
  if (any(is.na(xi) | ((x - xi) != 0))) {
    print(list(ERROR = "x not integer", x = x))
  }
  N <- length(x)
  xMax <- max(x)
  ndigits <- (floor(logb(xMax, base = 2)) + 1)
  Base.b <- array(NA, dim = c(N, ndigits))
  for (i in 1:ndigits) {
    Base.b[, ndigits - i + 1] <- (x %% b)
    x <- (x %/% b)
  }
  if (N == 1) Base.b[1, ] else Base.b
}



#' Component-wise Matrix of Log Marginal Likelihoods
#'
#' Calls CompLogML to create a matrix of number of possible components by
#' length(Nmissing) log marginal likelihoods. Calculates the log marginal
#' likehood of each possible marginal table for every value of Nmissing.
#'
#'
#' @param p Number of lists
#' @param delta Prior hyperparameter of the Dirichlet distribution.
#' @param Y The \code{2^k} matrix of list intersection counts.
#' @param Nmissing The vector of possible values for the missing cell.
#' @return A matrix of log marginal likelihoods.
#' @author James Johndrow \email{james.johndrow@@gmail.com} and Kristian Lum
#' \email{kl@@hrdag.org}
#' @keywords Bayesian model averaging marginal likelihood
#'
MakeCompMatrix <- function(p, delta, Y, Nmissing) {
  compLMLs <- matrix(0, nrow = 2^p - 1, ncol = length(Nmissing))
  bins <- integer.base.b(1:(2^p - 1), 2)
  for (i in 1:(2^p - 1)) {
    inds <- which(bins[i, ] == 1)
    D <- c(apply(Y, inds, sum))
    compLMLs[i, ] <- CompLogML(D, Nmissing, delta * 2^(p - sum(bins[i, ])))
  }
  return(compLMLs)
}



#' Bayesiam Model Averaging for Capture-Recapture
#'
#' This function averages over all decomposable graphical models for p lists.
#'
#' This is the main function in this package.  It performs capture-recapture
#' (or multiple systems estimation) using Bayesian model averaging as outlined
#' in Madigan and York (1997).
#'
#' Y can be created by the array() command from a vector that is ordered
#' lexigraphically by the cell names, e.g., c(x000, x001, x010, x011, x100,
#' x101, x110, x111).
#'
#' @param Y a \code{2^p} array of list intersection counts. See details.
#' @param Nmissing A vector of all possible values for the number of
#' individuals that appear on no list.
#' @param delta The hyper-parameter for the hyper-Dirichlet prior distribution
#' on list intersection probabilities. A smaller value indicates fewer prior
#' observations per cell. A suggested default is \code{2^-p}
#' @param graphs A pre-computed list of all decomposable graphical models for
#' \code{p} lists. These should be loaded using data(graphsp); see example.
#' Currently, this package includes a list of graphs for three, four, or five
#' lists.
#' @param logprior The log of the prior probability of each value in Nmissing.
#' If left blank, this will default to the -log(Nmissing).
#' @param log.prior.model.weights Prior weights on the graphs. This should be a
#' vector of length length(graphs).
#' @return This function returns a matrix of weights, where rows correspond to
#' models and columns correspond to values of Nmissing. Thus, the \code{ij}th
#' entry of the matrix is the posterior probability of the \code{i}th model and
#' the \code{j}th entry of Nmissing. Row sums return posterior probabilities by
#' model.Column sums return posterior probabilities by value of Nmissing.
#' @note This function is pretty robust relative to the more common log-linear
#' model approach to capture-recapture. It will not fail (or issue a numerical
#' warning) even if there are no overlaps among the lists. The user should take
#' care that there is adequate list overlap and that there are sufficient cases
#' in the stratum.
#' @author James Johndrow \email{james.johndrow@@gmail.com} and Kristian Lum
#' (kl@@hrdag.org)
#' @references Madigan, David, and Jeremy C. York. "Bayesian methods for
#' estimation of the size of a closed population."  Biometrika 84.1 (1997):
#' 19-31.
#' @keywords capture-recapture multiple systems estimation BMA
#' @examples
#'
#' #### 5 list example from M & Y ##########
#' delta <- .5
#' Y <- c(0, 27, 37, 19, 4, 4, 1, 1, 97, 22, 37, 25, 2, 1, 3, 5,
#'        83, 36, 34, 18, 3, 5, 0, 2, 30, 5, 23, 8, 0, 3, 0, 2)
#' Y <- array(Y, dim = c(2, 2, 2, 2, 2))
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#' data(graphs5)
#' weights <- bma.cr(Y, Nmissing, delta, graphs5)
#' plotPosteriorN(weights, N)
#'
#' ##### 3 list example from M & Y #######
#' Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
#' Y <- array(Y, dim = c(2, 2, 2))
#'
#' delta <- 1
#' a <- 13.14
#' b <- 55.17
#'
#'
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#'
#' logprior <- N * log(b) - (N + a) * log(1 + b) + lgamma(N + a) - lgamma(N + 1) - lgamma(a)
#'
#' data(graphs3)
#' weights <- bma.cr(Y, Nmissing, delta, graphs3, logprior)
#' plotPosteriorN(weights, N)
#' @export
bma.cr <- function(Y, Nmissing, delta, graphs,
                   logprior = NULL,
                   log.prior.model.weights = NULL) {
  if (is.null(logprior)) {
    logprior <- -log(sum(Y) + Nmissing)
  }

  Y[1] <- 0
  p <- length(dim(Y))

  # Precomputations
  compMat <- MakeCompMatrix(p, delta, Y, Nmissing)
  D <- lgamma(length(Y) * (delta)) - lgamma(Nmissing + sum(Y) + length(Y) * (delta))
  multinomialCoefficient <- lgamma(Nmissing + sum(Y) + 1) - sum(lgamma(Y[-1] + 1)) - lgamma(Nmissing + 1)

  # Compute log posterior for all models
  weights <- computeLogPostProbs(compMat, graphs, D, p)
  rowAdd(weights, multinomialCoefficient)
  rowAdd(weights, logprior)
  if (!is.null(log.prior.model.weights)) colAdd(weights, log.prior.model.weights)

  # Normalization
  expNormalize(weights)

  return(weights)
}



#' Plots Posterior Distribution of Nmissing
#'
#' Plots the model averaged posterior distribution of the total number of
#' elements (the solid line) and the contribution to the posterior of each of
#' the models (dotted lines)
#'
#'
#' @param weights The output of BMAfunction.
#' @param N N + Nmissing. Or, if you prefer, just Nmissing. The former shows
#' the posterior distribution of the total population size; the latter shows
#' the posterior distribution of the number of missing elements.
#' @param main the title of the plot
#' @return A plot.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords posterior distribution model averaging
#' @examples
#'
#' ##### 5 list example from M & Y #######
#'
#' delta <- .5
#' Y <- c(0, 27, 37, 19, 4, 4, 1, 1, 97, 22, 37, 25, 2, 1, 3, 5,
#'        83, 36, 34, 18, 3, 5, 0, 2, 30, 5, 23, 8, 0, 3, 0, 2)
#' Y <- array(Y, dim = c(2, 2, 2, 2, 2))
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#' data(graphs5)
#' weights <- bma.cr(Y, Nmissing, delta, graphs5)
#' plotPosteriorN(weights, N)
#'
#'
#' ##### 3 list example from M & Y #######
#' Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
#' Y <- array(Y, dim = c(2, 2, 2))
#'
#' delta <- 1
#' a <- 13.14
#' b <- 55.17
#'
#'
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#'
#' logprior <- N * log(b) - (N + a) * log(1 + b) + lgamma(N + a) - lgamma(N + 1) - lgamma(a)
#'
#' data(graphs3)
#' weights <- bma.cr(Y, Nmissing, delta, graphs3, logprior)
#' plotPosteriorN(weights, N)
#' @import graphics
#' @export
plotPosteriorN <- function(weights, N, main = NULL) {
  graphics::plot(N, apply(weights, 2, sum),
    type = "l", col = "black", lwd = 3,
    ylab = "Posterior Probability of N",
    xlab = "N",
    ylim = c(0, 1.25 * max(apply(weights, 2, sum)))
  )
  graphics::title(main)

  wts <- apply(weights, 1, sum)
  for (i in 1:nrow(weights)) {
    graphics::lines(N, weights[i, ], lwd = wts[i] * 3, lty = "dashed")
  }

  graphics::legend("topright",
    legend = c("Averaged Post. Prob.", "Post. Prob. By Model"),
    lty = c(1, 2), cex = .75
  )
}
