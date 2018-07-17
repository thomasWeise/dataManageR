#' @title Get the Names of All \code{\link{dataset}s} in a List
#' @description This function extracts all the names of the
#'   \code{\link{dataset}s} in a listand returns them in the same order as the
#'   \code{\link{dataset}s} appear.
#' @param datasets the list of  \code{\link{dataset}s}
#' @return the vector of names of the  \code{\link{dataset}s}
#' @export datasets.names
#' @include dataset.R
datasets.names <- function(datasets) vapply(X=datasets, FUN=function(ds) ds@name, FUN.VALUE="unnamed")
