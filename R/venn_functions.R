#' A Helper Function for venn3
#'
#' Used in venn3 to tell whether proposed points are inside of the given
#' circle.
#'
#'
#' @param ps a n x 2 matrix of coordinates.
#' @param x the x coordinate of the center of the circle.
#' @param y the y coordinate of the center of the circle.
#' @param r the radius of the circle.
#' @return a length n vector telling whether each row of ps is inside the given
#' circle.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords circle
#' @examples
#'
#'
#'
#' ps <- cbind(runif(100), runif(100))
#' plot(dga:::circle(0, 0, 1), type = "l")
#' inds <- dga:::circle.ind(ps, 0, 0, 1)
#' points(inds)
circle.ind <- function(ps, x, y, r) {
  dist <- ((ps[, 1] - x)^2 + (ps[, 2] - y)^2) < r^2
  return(dist)
}



#' A Helper Function to Tell Which Points Are Near the Boundary of a Circle
#'
#' Used in venn3 to tell which of the potential points to be plotted are near
#' the boundary of the circle defned by x, y, and r.
#'
#'
#' @param ps an n x 2 matrix of potential points.
#' @param x the x coordinate of the center of the circle.
#' @param y the y coordinate of the center of the circle.
#' @param r the radius of the circle
#' @return \item{inds}{tells which points are too close to the edge of the
#' circle. }
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords circle
#' @examples
#'
#'
#'
#' ps <- cbind(runif(100), runif(100))
#' inds <- dga:::remove.close(ps, .5, .5, .1)
remove.close <- function(ps, x, y, r) {
  dist <- ((ps[, 1] - x)^2 + (ps[, 2] - y)^2)
  keep <- abs(r^2 - dist) > .005
  return(keep)
}



#' A Helper Function Used in venn3
#'
#' Takes the parameters of a circle and returns points on its perimeter to be
#' plotted to make circles for a venn diagram.
#'
#'
#' @param x the x coordinate of the center of the circle.
#' @param y the y coordinate of the center of the circle.
#' @param r the radius of the circle
#' @return \item{inds}{the x,y coordinates of the periphery of a circle, to be
#' used in venn3. }
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords circle
#' @examples
#'
#'
#' plot(dga:::circle(0, 0, 1), type = "l")
circle <- function(x, y, r) {
  deg <- seq(0, 2 * pi + .2, .1)
  xs <- cos(deg) * r + x
  ys <- sin(deg) * r + y
  return(cbind(xs, ys))
}



#' Three List Venn Diagram
#'
#' A function that plots a venn diagram of 3 lists. One point is plotted in
#' each region for each record that falls into the corresponding list overlap.
#'
#'
#' @param overlap.counts A vector of length \code{2^3} that gives the number of
#' records in each overlap in lexicographic order, i.e. 001, 010, 011, 100,
#' etc.
#' @param main the title of the graph
#' @param num.test.points how many test points to generate as potentials to be
#' plotted in the circles.
#' @param p.cex the size of the points to be plotted
#' @param write_numbers indicates whether to print the number of points in each
#' region.
#' @param t.cex the size of the text to write the numbers.
#' @param cex.main the size of the title
#' @return a 3-way venn diagram with points inside of each segment representing
#' the number of records on each list overlap.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords venn diagram
#' @examples
#'
#'
#' overlap.counts <- rpois(8, 30)
#' venn3(overlap.counts, main = "example diagram")
#' @export
venn3 <- function(overlap.counts, main = NULL, num.test.points = 100000, p.cex = .75, write_numbers = FALSE, t.cex = 1.25, cex.main = 1) {
  width <- .4
  graphics::plot(NA, NA,
    ylim = c(-width, width), xlim = c(-width, width), xaxt = "n", yaxt = "n",
    ylab = "", xlab = "", bty = "n", main = main, cex.main = cex.main
  )
  xs <- c(.37, .63, .5) - .5
  ys <- c(.4, .4, .6) - .5
  rs <- c(.2, .2, .2)
  # draw.circle(xs, ys, rs, border="black")

  ps <- cbind(stats::runif(num.test.points, -width, width), stats::runif(num.test.points, -width, width))
  keep <- remove.close(ps, xs[1], ys[1], rs[1])
  ps <- ps[keep, ]
  keep <- remove.close(ps, xs[2], ys[2], rs[2])
  ps <- ps[keep, ]
  keep <- remove.close(ps, xs[3], ys[3], rs[3])
  ps <- ps[keep, ]

  c1 <- circle.ind(ps, xs[1], ys[1], rs[1])
  c2 <- circle.ind(ps, xs[2], ys[2], rs[2])
  c3 <- circle.ind(ps, xs[3], ys[3], rs[3])

  # intersect.id <- paste(c1*1, c2*1, c3*1, sep = '')
  X <- integer.base.b(0:7)
  cols <- c("gray", "red", "blue", "purple", "gold", "darkorange", "green3", "chocolate4")
  out.points <- cbind(stats::runif(overlap.counts[1], -width, -width + .2), stats::runif(overlap.counts[1], width - .2, width))
  for (i in 1:length(overlap.counts)) {
    good.points <- c1 == X[i, 1] & c2 == X[i, 2] & c3 == X[i, 3]
    if (sum(good.points) < overlap.counts[i]) {
      print(i)
      print("warning: not enough points, try to increase num.test.points")
    }
    tmp.ps <- ps[good.points, ]
    tmp.ps <- tmp.ps[1:overlap.counts[i], ]
    points(tmp.ps, col = cols[i], pch = 19, cex = p.cex)
  }

  graphics::lines(circle(xs[1], ys[1], rs[1]), lwd = 2)
  graphics::lines(circle(xs[2], ys[2], rs[2]), lwd = 2)
  graphics::lines(circle(xs[3], ys[3], rs[3]), lwd = 2)

  if (write_numbers) {
    num.x <- c(-.5, .2, -.2, 0, 0, .1, -.1, 0)
    num.y <- c(.5, -.15, -.15, -.15, .15, .025, .025, -.05)
    text(num.x, num.y, labels = overlap.counts, cex = t.cex)
  }
}



#' A Helper Function Used by Venn4 to Define the Perimeter of an Ellipse
#'
#' Draws the ellipses used in venn4.
#'
#'
#' @param x the x coordinate of the center of the ellipse.
#' @param y the y coordinate of the center of the ellipse.
#' @param a the x-direction radius.
#' @param b the y-direction radius.
#' @param alpha the angle of rotation of the ellipse
#' @return points that define the perimeter of an ellipse.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords ellipse
#' @examples
#'
#'
#' plot(dga:::ellipse(0, 0, .5, .2, 1))
ellipse <- function(x, y, a, b, alpha) {
  deg <- seq(0, 2 * pi + .2, .1)
  xs <- a * cos(deg)
  ys <- b * sin(deg)
  mat <- cbind(xs, ys)
  rot.mat <- rbind(c(cos(alpha), sin(alpha)), c(-sin(alpha), cos(alpha)))
  out.mat <- mat %*% rot.mat
  out.mat[, 1] <- out.mat[, 1] + x
  out.mat[, 2] <- out.mat[, 2] + y
  return(out.mat)
}



#' A Helper Function Used by Venn4
#'
#' Takes potential points to be plotted in the venn diagrams and returns
#' whether the point is inside or outside of the ellipse described by x, y, a,
#' b, and alpha.
#'
#'
#' @param ps a n x 2 matrix of coordinates.
#' @param x the x coordinate of the center of the ellipse.
#' @param y the y coordinate of the center of the ellipse.
#' @param a the x-radius of the ellipse.
#' @param b the y-radius of the ellipse.
#' @param alpha the angle of rotation of the ellipse
#' @return a length n vector indicating whether each point is inside the
#' ellipse.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords ellipse
#' @examples
#'
#'
#'
#' ## The function is currently defined as
#' ps <- cbind(runif(100), runif(100))
#' plot(dga:::ellipse(0, 0, .5, .3, 0), type = "l")
#' inds <- dga:::ellipse.ind(ps, 0, 0, .5, .3, 0)
#' points(inds)
ellipse.ind <- function(ps, x, y, a, b, alpha) {
  check <- (cos(alpha) * (ps[, 1] - x) + sin(alpha) * (ps[, 2] - y))^2
  check <- check / a^2
  check2 <- (sin(alpha) * (ps[, 1] - x) - cos(alpha) * (ps[, 2] - y))^2
  check2 <- check2 / b^2

  out <- (check + check2) < 1
  return(out)
}



#' A Helper Function to Tell Which Points are Near the Boundary of the Ellipse
#'
#' A helper function.
#'
#'
#' @param ps an n x 2 matrix of potential points.
#' @param x the x coordinate of the center of the ellipse.
#' @param y the y coordinate of the center of the ellipse.
#' @param a the x-radius of the ellipse.
#' @param b the y-radius of the ellipse.
#' @param alpha the angle of rotation of the ellipse.
#' @return \item{inds}{a vector of length nrow(ps) that tells whether each row
#' of ps is near the border of the ellipse defined by x,y,a,b, and alpha. }
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords ellipse
#' @examples
#'
#'
#' ## The function is currently defined as
#' ps <- cbind(runif(100), runif(100))
#' inds <- dga:::remove.close.ellipse(ps, .5, .5, .1, .3, 1)
remove.close.ellipse <- function(ps, x, y, a, b, alpha) {
  check <- (cos(alpha) * (ps[, 1] - x) + sin(alpha) * (ps[, 2] - y))^2
  check <- check / a^2
  check2 <- (sin(alpha) * (ps[, 1] - x) - cos(alpha) * (ps[, 2] - y))^2
  check2 <- check2 / b^2

  final <- check + check2
  keep <- final < .85 | final > 1.15
  return(keep)
}



#' Four List Venn Diagram
#'
#' A function that plots a venn diagram of 4 lists. One point is plotted in
#' each region for each record that falls into the corresponding list overlap.
#'
#'
#' @param overlap.counts A vector of length \code{2^4} that gives the number of
#' records in each overlap in lexicographic order, i.e. 0000, 0001, 0010, 0011,
#' 0100, etc.
#' @param main the title of the graph
#' @param num.test.points how many test points to generate as potentials to be
#' plotted in the circles.
#' @param p.cex the size of the points to be plotted
#' @param cex.main the size of the title
#' @return A venn diagram of the list overlap structure for four lists. Each
#' region of the plot contains points representing each record in that list
#' intersection.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords venn diagram
#' @examples
#'
#'
#' overlap.counts <- rpois(16, 50)
#' venn4(overlap.counts, main = "example diagram")
#' @export
venn4 <- function(overlap.counts, main = NULL, num.test.points = 100000, p.cex = .75, cex.main = 1) {
  ### draw 4 ellipses
  xs <- c(0, -.25, .25, 0)
  ys <- c(.1, -.1, -.1, .1)
  as <- rep(.35, 4)
  bs <- rep(.65, 4)
  alphas <- c(1, 1, -1, -1)
  ell <- ellipse(xs[1], ys[1], as[1], bs[1], alphas[1])
  graphics::plot(ell,
    type = "l", xlim = c(-.8, .8), ylim = c(-.8, .8),
    xaxt = "n", yaxt = "n", xlab = "", ylab = "",
    main = main, lwd = 2, cex.main = cex.main
  )
  ell2 <- ellipse(xs[2], ys[2], as[2], bs[2], alphas[2])
  graphics::lines(ell2, lwd = 2)
  ell3 <- ellipse(xs[3], ys[3], as[3], bs[3], alphas[3])
  graphics::lines(ell3, lwd = 2)
  ell4 <- ellipse(xs[4], ys[4], as[4], bs[4], alphas[4])
  graphics::lines(ell4, lwd = 2)

  width <- .8
  ps <- cbind(stats::runif(num.test.points, -width, width), stats::runif(num.test.points, -width, width))

  ## remove points from edges
  ps <- cbind(stats::runif(num.test.points, -width, width), stats::runif(num.test.points, -width, width))
  keep <- remove.close.ellipse(ps, xs[1], ys[1], as[1], bs[1], alphas[1])
  ps <- ps[keep, ]
  keep <- remove.close.ellipse(ps, xs[2], ys[2], as[2], bs[2], alphas[2])
  ps <- ps[keep, ]
  keep <- remove.close.ellipse(ps, xs[3], ys[3], as[3], bs[3], alphas[3])
  ps <- ps[keep, ]
  keep <- remove.close.ellipse(ps, xs[4], ys[4], as[4], bs[4], alphas[4])
  ps <- ps[keep, ]


  ### check which points are where
  e1 <- ellipse.ind(ps, xs[1], ys[1], as[1], bs[1], alphas[1])
  e2 <- ellipse.ind(ps, xs[2], ys[2], as[2], bs[2], alphas[2])
  e3 <- ellipse.ind(ps, xs[3], ys[3], as[3], bs[3], alphas[3])
  e4 <- ellipse.ind(ps, xs[4], ys[4], as[4], bs[4], alphas[4])


  #### plot points
  X <- integer.base.b(0:(2^4 - 1))
  # cols <- c('gray', 'red', 'blue', 'purple', 'gold', 'darkorange', 'green3', 'chocolate4')
  # cols <- 1:16
  c1 <- c(.75, 0, 0, 0.25)
  c2 <- c(0, .75, 0, .25)
  c3 <- c(0, 0, .75, .25)
  c4 <- c(0, 0, 0, .5)

  cols <- c(
    "gray", "red", "gold", "orange", "blue", "purple", "green3", "saddlebrown", "palegreen1",
    "lightsalmon3", "olivedrab2", "sienna2", "seagreen", "slateblue3",
    "aquamarine", "gray29"
  )
  # c1 <- c(.1, .4, .6)
  # c2 <- c(.2, .4, .2)
  # c3 <- c(.3, .4, .1)
  # c4 <- c(.4, .4, .1)
  # out.points <- cbind(runif(overlap.counts[1], -width, -width + .2), runif(overlap.counts[1], width - .2, width))
  for (i in 1:length(overlap.counts)) {
    good.points <- e1 == X[i, 1] & e2 == X[i, 2] & e3 == X[i, 3] & e4 == X[i, 4]
    if (sum(good.points) < overlap.counts[i]) {
      print(i)
      print("warning: not enough points, try to increase num.test.points")
    }
    tmp.ps <- ps[good.points, ]
    tmp.ps <- tmp.ps[1:overlap.counts[i], ]
    # col <- c(.5*X[i,1], .5*X[i,2], .5*X[i,3], .5 + .4*X[i,4])
    # col <- c1*X[i,1] + c2*X[i,2] + c3*X[i,3] + c4*X[i,4]
    if (sum(X[i, ]) == 0) {
      col <- c(.5, .5, .5, 1)
    }
    points(tmp.ps, col = cols[i], cex = p.cex, pch = 16) # rgb(min(col[1],1), min(col[2],1), min(col[3],1), min(col[4],1)), pch = 19, cex = p.cex)
  }
  graphics::lines(ell, lwd = 2)
  graphics::lines(ell2, lwd = 2)
  graphics::lines(ell3, lwd = 2)
  graphics::lines(ell4, lwd = 2)
}
