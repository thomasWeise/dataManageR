#' @title Get the Names of All Datasets in a Vector
#' @description This function extracts all the names of the
#'   \code{\link{dataset}s} in a listand returns them in the same order as the
#'   \code{\link{dataset}s} appear.
#' @param datasets the list of  \code{\link{dataset}s}
#' @return the vector of names of the  \code{\link{dataset}s}
#' @export datasets.names.get
#' @include dataset.R
datasets.names.get <- function(datasets) vapply(X=datasets, FUN=function(ds) ds@name, FUN.VALUE="unnamed")
