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

p <- 5
delta <- .5
Y = c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
Y <- array(Y, dim=c(2,2,2,2,2))
Nmissing <- 1:300
N <- Nmissing + sum(Y)



load(paste("~/Dropbox (Personal)/Projects/Capture-Recapture/DGA/R/output/graphs", p, '.Rdata', sep='')) #loads graphs
#######
time <- proc.time()
weights <- BMAfunction(Y,  Nmissing, delta, graphs)
time.end <- proc.time()
time.end - time
plotPosteriorN(weights, N)



plot(Nmissing + sum(Y), apply(weights, 2, sum))
for(i in 1:nrow(weights)){
  lines(Nmissing + sum(Y), weights[i,])
}

tmp <- apply(weights,1, sum)
round(tmp[tmp> .01], 3)
