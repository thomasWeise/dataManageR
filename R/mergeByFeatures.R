#' @include dataset.R
#' @include joinNames.R
#' @include mergeBySelection.R

#' @title Create a Combination or Selection of \code{\link{dataset}s}
#' @description Merge a set of \code{\link{dataset}s} according to a given name transformation.
#' @param datasets the list of instances of \code{\link{dataset}}
#' @param features.ignore a vector of names of features to delete, or
#'   \code{NULL} if no features should explicitly be deleted
#' @param features.keep a vector of the names of the (only) features to keep, or
#'   \code{NULL} if all non-deleted features should be kept
#' @param data.name a function used to join names of datasets that now have
#'   identical features, see \code{\link{datasets.names.join}} for documentation
#' @param data.merge a function for merging the \code{data} lists; by default
#'   \code{unlist(x, recursive=FALSE)}
#' @param data.create a function for creating new datasets, by default
#'   \code{\link{dataset.new}}
#' @return a list of two components, \code{datasets} and \code{selection}. The
#'   \code{datasets} component contains instances of \code{\link{dataset}} where
#'   all names have been transformed according to \code{data.name} and the features
#'   have been selected according to \code{features.ignore} and
#'   \code{features.keep} where the \code{data} members of instances with the
#'   same features have been merged. The \code{selection} component contains,
#'   for each element of \code{datasets}, a vector with indexes into the input
#'   \code{datasets} list from where its components stem
#' @export datasets.merge.by.features
datasets.merge.by.features <- function(datasets,
                                       features.ignore=NULL,
                                       features.keep=NULL,
                                       data.name=datasets.names.join,
                                       data.merge=.data.merge,
                                       data.create=dataset.new) {
  # if there are no datasets, we are done here
  n <- length(datasets);
  if(length(n) <= 0L) {
    .tmp <- list();
    return(list(datasets=.tmp, selection=.tmp));
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
    selection <- seq_len(n);
    return(list(datasets = unname(unlist(lapply(X=selection, FUN=function(i) {
      return(data.create(name=data.name(datasets[[i]]@name),
                   features=features.all[[i]],
                   data=datasets[[i]]@data));
    }), recursive = TRUE)),
    selection=selection));
  }

  # create the selections: for each unique feature composition, compute the
  # vector of indices of datasets that should be merged
  selection <- lapply(X=features.unique, FUN=function(features)
                which(vapply(X=features.all, FUN=identical, FUN.VALUE=FALSE, features)));

  # invoke the merging algorithm
  result <- .datasets.merge.by.selection(datasets=datasets,
                                         selection=selection,
                                         data.name=data.name,
                                         features.unique=features.unique,
                                         data.merge=data.merge,
                                         data.create=data.create);
  result <- force(result);
  return(list(datasets=result, selection=selection));
}
