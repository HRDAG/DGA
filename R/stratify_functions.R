#' Transforms Records to List Intersection Counts by Stratum
#'
#' Helps you to create list overlaps in the correct order to be used in bma.cr.
#' This function also does some of the heavy lifting to stratify records by
#' time (date, etc.) and other variables.
#'
#'
#' @param overlaps a data frame that tells whether the i'th record appears on
#' the j'th lists, where n is the total number of sampled elements and p is the
#' number of lists. For example, if the [3,2] entry is 1, then the third
#' element appeared on the second list. If it is zero, then the third element
#' did NOT appear on the second list.
#' @param dates record dates, in identical row oder to overlaps. This must be a
#' chron object. Do not include this if you don't want to stratify by time.
#' @param locations record locations, though unlike the dates, there is nothing
#' special about the type that would prevent you from using any other variable
#' type to stratify by here. Do not include this unless you want to stratify by
#' the factor you include here.
#' @param demographics record demographic variables. Like locations, there is
#' nothing specific to this that requires this be demographic. This should be a
#' factor. Do not incude this unless you want to stratify by this factor.
#' @param date.defs how you'd like to stratify by date. This defaults to
#' "monthly". Other options are "weekly", "daily", and "yearly". If you enter
#' an integer (k) instead of one of these options, the data will be stratified
#' into blocks of size k days.
#' @param loc.defs How to divide up all of the levels of locations into groups.
#' e.g. if locations has levels A, B, and C, and you'd like to stratify so that
#' A and B are one strata and C is another, input loc.defs = list(g1 = c('A',
#' 'B'), g2 = c('C')). If this is left as NULL, each level will be put into its
#' own stratum.
#' @param demog.defs Similar to loc.defs. Same format. Including both just
#' allows you to stratify along two dimensions.
#' @param start.date A chron object of one date.  This gives the date of
#' earliest record we want to include. If NULL, this defaults to the earliest
#' record in the dataset.
#' @param end.date a chron object of one date. This gives the date of the
#' latest record to be included. If NULL, this defaults to the latest record in
#' the dataset. This can only be included if dates are given.
#' @return \item{overlap.counts}{ a data frame where each row gives the list
#' intersection counts that can be used in bma.cr} \item{source.counts}{ a data
#' frame that gives the total number of records by each data source and
#' stratum. }
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords stratification
#' @examples
#'
#'
#' require(chron)
#' N <- 10000
#' overlaps <- data.frame(l1 = rbinom(N, 1, .5), l2 = rbinom(N, 1, .5), l3 = rbinom(N, 1, .5))
#' dates <- paste(
#'   rep(2015, N), "-", sample(1:12, N, replace = TRUE), "-",
#'   sample(1:28, N, replace = TRUE)
#' )
#' dates <- chron(dates, format = c(dates = "y-m-d"))
#' locations <- sample(c("A", "B", "C", "D"), N, replace = TRUE)
#'
#' # Aggregate only by week:
#' make.strata(overlaps, dates, date.def = "weekly")
#'
#' # Aggregate by year and location, where locations are not grouped:
#' make.strata(overlaps, dates, date.def = "yearly", locations)
#'
#' # Aggregate by 2 day increments and location, where there are unique location levels
#' #       A, B, C, and D and locations A and B are in group 1
#' #       and locations C and D are in group 2.
#' loc.defs <- list("g1" = c("A", "B"), "g2" = c("C", "D"))
#' make.strata(overlaps, dates, date.def = 2, locations, loc.defs = loc.defs)
#' # Aggregate by demographic (sex) only, where sex takes values M, F, A, NA, and U
#' #       and we would like to group these as M, F, and other.
#' sex <- sample(c("M", "F", "A", NA, "U"),
#'   prob = c(.4, .4, .1, .05, .05),
#'   N, replace = TRUE
#' )
#' demog.defs <- list("M" = "M", "F" = "F", "Other" = c("A", NA, "U"))
#' make.strata(overlaps, demographics = sex, demog.defs = demog.defs)
#' @export
make.strata <- function(overlaps, dates = NULL, locations = NULL,
                        demographics = NULL, date.defs = "monthly", loc.defs = NULL,
                        demog.defs = NULL, start.date = NULL, end.date = NULL) {
  date.cat <- NULL
  loc.cat <- NULL
  demog.cat <- NULL


  # get date categories
  if (!is.null(dates)) {
    if (!is.null(start.date)) {
      min.date <- start.date
    } else {
      min.date <- min(dates)
    }
    if (!is.null(end.date)) {
      max.date <- end.date
    } else {
      max.date <- max(dates)
    }
    if (date.defs == "yearly") {
      date.cat <- chron::years(dates)
    }
    if (date.defs == "monthly") {
      date.cat <- paste(months(dates), chron::years(dates))
    }
    if (date.defs == "weekly") {
      wd <- weekdays(dates)
      min.sun <- (min.date - 0:6)
      min.sun <- min.sun[weekdays(min.sun) == "Sun"]
      max.sun <- max.date + 0:6
      max.sun <- max.sun[weekdays(max.sun) == "Sun"]
      intervals <- c(seq(0, max.sun - min.sun, 7))
      cuts <- c(min.sun - 7, min.sun + intervals, max.sun + 7) - 1
      cut.labels <- cuts + 1
      date.cat <- cut(dates, cuts, labels = cut.labels[1:(length(cut.labels) - 1)])
    }
    if (date.defs == "daily") {
      date.cat <- dates
    }
    if (date.defs == "dayofweek") {
      date.cat <- weekdays(dates)
    }
    if (is.numeric(date.defs)) {
      intervals <- seq(0, max.date - min.date, date.defs) - 1
      cuts <- c(min.date + intervals, max.date + date.defs)
      cut.labels <- cuts + 1
      date.cat <- cut(dates, cuts, labels = cut.labels[1:(length(cuts) - 1)])
    }
  }

  if (!is.null(locations)) {
    if (is.null(loc.defs)) {
      loc.cat <- locations
    } else {
      loc.cat <- nrow(overlaps)
      for (loc in names(loc.defs)) {
        loc.cat[is.element(locations, loc.defs[[loc]])] <- loc
      }
    }
  }

  if (!is.null(demographics)) {
    if (is.null(demog.defs)) {
      demog.cat <- demographics
    } else {
      demog.cat <- nrow(overlaps)
      for (demog in names(demog.defs)) {
        demog.cat[is.element(demographics, demog.defs[[demog]])] <- demog
      }
    }
  }

  strata <- paste(date.cat, loc.cat, demog.cat)
  nlist <- ncol(overlaps)
  if (nlist > 1) {
    capt.history <- apply(t(overlaps) * 2^((nlist - 1):0), 2, sum)
    strata.history <- sapply(split(capt.history, strata), cfunction, nlist = nlist)
    source.counts <- t(sapply(split(overlaps, strata), sfunction))
  } else {
    strata.history <- t(table(strata))
    source.counts <- strata.history
  }

  # get by-list region counts
  out <- list()
  out$overlap.counts <- t(strata.history)
  out$source.counts <- source.counts
  return(out)
}



#' A Helper Function for make.strata
#'
#' A helper function used in make.strata to make list overlap counts.
#'
#'
#' @param x capture histories, transformed from binary to decimal
#' @param nlist the number of lists
#' @return a table of the number of records with each capture history
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords stratification
#' @examples
#'
#'
#' ## The function is currently defined as
#' cfunction <- function(x, nlist) {
#'   out <- table(c(x, 0:(2^nlist - 1))) - 1
#' }
cfunction <- function(x, nlist) {
  out <- table(c(x, 0:(2^nlist - 1))) - 1
}



#' A Helper Function for make.strata.
#'
#' This is the simplest function ever. It's just an apply to sum across
#' columns.
#'
#'
#' @param x capture histories, as numbers
#' @return apply(x, 2, sum)
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords stratification
#' @examples
#'
#'
#'
#' ## The function is currently defined as
#' sfunction <- function(x) {
#'   out <- apply(x, 2, sum)
#' }
sfunction <- function(x) {
  out <- apply(x, 2, sum)
}



#' Checks Each Stratum for Suitability for Capture-Recapture
#'
#' Takes in list intersection counts and source list totals as produced by
#' make.strata. It then checks whether there are between three and five lists,
#' whether all of the lists are non-empty, and whether all of the lists overlap
#' with some other list.
#'
#'
#' @param strata A list of list overlaps and source countsin the format of the
#' output of make.strata. list.overlaps contains a data frame of list overlaps
#' by stratum. source.counts contains the number of records by source and
#' stratum.
#' @return A boolean indicating whether any serious problems have been found
#' with the strata.
#' @note This does not issue a warning for cases where some subset of lists is
#' not connected to the others, e.g. Lists A and B have overlap with each
#' other, lists C and D have overlap with each other, but no records from A or
#' B overlap with lists C or D. We suggest that you examine the list
#' intersection counts manually as well.
#' @author Kristian Lum \email{kl@@hrdag.org}
#' @keywords list overlaps
#' @examples
#'
#'
#' library(chron)
#'
#' N <- 1000
#' overlaps <- data.frame(l1 = rbinom(N, 1, .5), l2 = rbinom(N, 1, .5), l3 = rbinom(N, 1, .5))
#' dates <- paste(
#'   rep(2015, N), "-", sample(1:12, N, replace = TRUE), "-",
#'   sample(1:28, N, replace = TRUE)
#' )
#' dates <- chron(dates, format = c(dates = "y-m-d"))
#' locations <- sample(c("A", "B", "C", "D"), N, replace = TRUE)
#'
#' # Aggregate only by week:
#' strata <- make.strata(overlaps, dates, date.def = "weekly")
#' check <- check.strata(strata)
#' @export
check.strata <- function(strata) {
  flag <- TRUE
  # first check that this has the right stuff in it
  stopifnot(names(strata) == c("overlap.counts", "source.counts"))

  # check to make sure we have a number of lists we can handle
  num.lists <- ncol(strata$source.counts)
  if (num.lists < 3) {
    print("You have fewer than 3 lists! Are you sure you want to do this?")
    flag <- FALSE
  }
  if (num.lists > 5) {
    print("Sorry! We have only pre-computed all of the graphs for three, four, and five lists!
          Come back later! Or sub-select three, four, or five of your lists to use this package.")
    flag <- FALSE
  }

  # check to make sure that none of the lists are empty
  zeroes <- apply(strata$source.counts == 0, 1, sum) > 0
  if (sum(zeroes) > 0) {
    print(paste("these strata have empty lists:", rownames(strata$source.counts)[zeroes]))
    flag <- FALSE
  }

  small <- apply(strata$source.counts < 10, 1, sum) > 0
  if (sum(small) > 0) {
    print(paste(
      "Proceed with caution. These strata have lists with very few (less than 10) records:",
      rownames(strata$source.counts)[small]
    ))
    flag <- FALSE
  }

  # check to make sure that every list overlaps with at least one other list
  X <- integer.base.b(0:(2^num.lists - 1))
  bad.strata <- NULL
  for (i in 1:num.lists) {
    inds <- X[, i] == 1 & apply(X[, -i], 1, sum) > 0
    two.way.overlaps <- apply(strata$overlap.counts[, inds], 1, sum)
    bad.strata <- c(bad.strata, rownames(strata$overlap.counts)[two.way.overlaps == 0])
  }

  if (length(bad.strata) > 0) {
    print(paste("These strata have lists that do not intersect with any other lists: ", bad.strata))
    flag <- FALSE
  }

  # Eventually, check to see if there are any "islands"

  if (flag) {
    print("I didn't find any obvious problems. Onward!")
  }
  return(flag)
}
