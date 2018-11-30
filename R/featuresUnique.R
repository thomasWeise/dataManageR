#' @include dataset.R

#' @title Find all the Unique Combinations of Feature Values and the
#'   Corresponding Datasets
#' @description Search the features of a set of \code{\link{dataset}s} for
#'   unique combinations of values of the specified features.
#' @param datasets the list of instances of \code{\link{dataset}}
#' @param features.ignore a vector of names of features to ignore, or
#'   \code{NULL} if no features should explicitly be ignored
#' @param features.keep a vector of the names of the (only) features to
#'   consider, or \code{NULL} if all non-ignored features should be considered
#' @return a list of two components, \code{features} and \code{selection}. The
#'   \code{features} component contains the lists of unique feature value
#'   combinations. The \code{selection} component contains, for each element of
#'   \code{features}, a vector with indexes into the input \code{datasets} list
#'   where elements with these features are located
#' @export datasets.features.unique
datasets.features.unique <- function(datasets,
                                     features.ignore=NULL,
                                     features.keep=NULL) {

  # if there are no datasets, we are done here
  n <- length(datasets);
  if(length(n) <= 0L) {
    .tmp <- list();
    return(list(features=.tmp, selection=.tmp));
  }

  # check the parameters
  if(length(features.ignore) <= 0L) {
    features.ignore <- NULL;
  }
  if(length(features.keep) <= 0L) {
    features.keep <- NULL;
  }

  # compute all the feature vectors
  features.all <- lapply(X=datasets, FUN=function(ds) {
    features <- ds@features;
    if(!is.null(features.keep)) {
      features <- features[features.keep];
    }
    if(!is.null(features.ignore)) {
      features <- features[is.na(match(names(features), features.ignore))];
    }
    return(features);
    });

  # find the unique feature vectors
  features.unique <- unique(features.all);
  m               <- length(features.unique);

  # if the list length is the same, we just update the names
  if(m >= n) {
    result <- list(features=features.all, selection=seq_len(n));
    result <- force(result);
    return(result);
  }

  # create the selections: for each unique feature composition, compute the
  # vector of indices of datasets that should be merged
  selection <- lapply(X=features.unique, FUN=function(features)
                which(vapply(X=features.all, FUN=identical, FUN.VALUE=FALSE, features)));

  # create the result
  result <- list(features=features.unique, selection=selection);
  result <- force(result);
  return(result);
}
