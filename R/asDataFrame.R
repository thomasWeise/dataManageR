#' @title Convert the \code{\link{dataset}} Features, Feature Names, and
#'   \code{\link{dataset}} Names into a \code{\link{data.frame}}
#' @description Create a data frame which represents all the meta-information
#'   about the \code{\link{dataset}s}.
#' @param datasets the \code{\link{dataset}s}
#' @param stringsAsFactors convert strings to factors if \code{TRUE}, keep them
#'   as strings if \code{FALSE}
#' @return the data frame
#' @export datasets.as.data.frame
#' @include featureNames.R
#' @include featureValues.R
#' @include getNames.R
datasets.as.data.frame <- function(datasets, stringsAsFactors=TRUE) {
  features <- datasets.feature.names(datasets);
  values <- lapply(X=features, FUN=function(feature) datasets.feature.values(datasets, feature));
  values$stringsAsFactors <- stringsAsFactors;
  frame <- do.call(cbind.data.frame, values);
  names(frame) <- features;
  rownames(frame) <- datasets.names.get(datasets);
  return(frame);
}
