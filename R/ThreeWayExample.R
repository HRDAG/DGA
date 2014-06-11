#     DGA does capture-recapture estimation using decomposable graphical models
#
# Copyright (C) 2014, Human Rights Data Analysis Group (HRDAG)
#     https://hrdag.org
#
# This file is part of DGA.
#
# snap is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# snap is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with DGA.  If not, see <http://www.gnu.org/licenses/>.

#test madigan and york 3 way spina bifida example
source("~/Dropbox (Personal)/Projects/Capture-Recapture/DGA/R/src/BMAfunctions.R")
#multcoef <-  lgamma(apply(D, 1, sum) + delta) - apply(lgamma(D[,-1] +delta), 1, sum) - lgamma(D[,1] + 1)

###stuff to get this tested
# first example from Madigan and York
p <- 3
#Y <- c(0, 49, 247, 142, 60, 4, 112, 12)
Y <- c(0, 60, 49, 4, 247, 112, 142, 12)
Y <- array(Y, dim=c(2,2,2))

delta <- 1
a <- 13.14
b <- 55.17


Nmissing <- 1:300
N <- Nmissing + sum(Y)

logprior <- N*log(b) - (N + a)*log(1 + b)  + lgamma(N + a) - lgamma(N + 1) - lgamma(a)
load(paste("~/Dropbox (Personal)/Projects/Capture-Recapture/DGA/R/output/graphs", p, '.Rdata', sep='')) #loads graphs
#######

weights <- BMAfunction(Y,  Nmissing, delta, graphs, logprior)
plotPosteriorN(weights, N)
plotTopModels(graphs, weights, p, how.many = 3)

round(apply(weights, 1, sum), 3)