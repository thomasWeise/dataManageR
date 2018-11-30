#' @include dataset.R
#' @include joinNames.R
#' @include mergeBySelection.R
#' @include featuresUnique.R

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

  # get the feature combinations
  found <- datasets.features.unique(datasets=datasets,
                                    features.ignore=features.ignore,
                                    features.keep=features.keep);
  found <- force(found);
  m     <- length(found$features);

  # if the list length is the same, we just update the names
  if(m >= n) {
    ds <- unname(unlist(lapply(X=found$selection, FUN=function(i) {
      return(data.create(name=data.name(datasets[[i]]@name),
                         features=found$features[[i]],
                         data=datasets[[i]]@data));
    }), recursive = TRUE));
    ds <- force(ds);

    result <- list(datasets=ds, selection=found$selection);
    result <- force(result);
  }

  # invoke the merging algorithm
  result <- .datasets.merge.by.selection(datasets=datasets,
                                         selection=found$selection,
                                         data.name=data.name,
                                         features.unique=found$features,
                                         data.merge=data.merge,
                                         data.create=data.create);
  result <- force(result);
  result <- list(datasets=result, selection=found$selection);
  result <- force(result);
  return(result);
}
