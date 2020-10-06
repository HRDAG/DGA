library(dga)

# Load version 1.2 of the bma.cr function
dga1.2_url <- "https://raw.githubusercontent.com/cran/dga/05b5d99436dac1dd5aa796d466f1cf793b6460e1/R/BMAfunctions.R"
dga1.2 <- new.env()
source(dga1.2_url, local = dga1.2)
bma.cr.1.2 <- get("bma.cr", env = dga1.2)

test_that("5 lists example against dga 1.2", {
  ##### 5 list example from M & Y #######
  delta <- .5
  Y <- c(0, 27, 37, 19, 4, 4, 1, 1, 97, 22, 37, 25, 2, 1, 3, 5, 83, 36, 34, 18, 3, 5, 0, 2, 30, 5, 23, 8, 0, 3, 0, 2)
  Y <- array(Y, dim = c(2, 2, 2, 2, 2))
  Nmissing <- 1:300
  N <- Nmissing + sum(Y)
  data(graphs5)

  weights <- bma.cr(Y, Nmissing, delta, graphs5)
  weights1.2 <- bma.cr.1.2(Y, Nmissing, delta, graphs5)

  expect_equal(weights, weights1.2)
})

test_that("3 lists example against dga 1.2", {
  ##### 3 list example from M & Y #######
  Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
  Y <- array(Y, dim = c(2, 2, 2))

  delta <- 1
  a <- 13.14
  b <- 55.17

  Nmissing <- 1:300
  N <- Nmissing + sum(Y)

  logprior <- N * log(b) - (N + a) * log(1 + b) + lgamma(N + a) - lgamma(N + 1) - lgamma(a)

  data(graphs3)
  weights <- bma.cr(Y, Nmissing, delta, graphs3, logprior)
  weights1.2 <- bma.cr.1.2(Y, Nmissing, delta, graphs3, logprior)

  expect_equal(weights, weights1.2)
})

test_that("3 lists example against Madigan and York", {
  ##### 3 list example from M & Y #######
  Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
  Y <- array(Y, dim = c(2, 2, 2))

  delta <- 1
  a <- 13.14
  b <- 55.17

  Nmissing <- 1:300
  N <- Nmissing + sum(Y)

  logprior <- N * log(b) - (N + a) * log(1 + b) + lgamma(N + a) - lgamma(N + 1) - lgamma(a)

  data(graphs3)
  weights <- bma.cr(Y, Nmissing, delta, graphs3, logprior)

  model_weights <- rowSums(weights)
  top_model <- weights[order(-model_weights)[1], ]

  # Check top model posterior probability, Bayes estimator, posterior mode and quantiles
  expect_equal(round(sum(top_model), 2), 0.37)
  expect_equal(round(mean(top_model / N) / mean(top_model / N^2)), 731)
  expect_equal(N[which.max(top_model)], 729)

  # Check model averaing Bayes estimator and posterior mode
  avg <- colSums(weights)
  expect_equal(round(mean(avg / N) / mean(avg / N^2)), 731)
  expect_equal(N[which.max(avg)], 728)
})
