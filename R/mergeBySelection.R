# the default data merger
.data.merge <- function(x) unlist(x, recursive=FALSE)

# the internal merge-by-selection method, which already requires having the right features
#' @include makeNamesUnique.R
.merge.by.selection <- function(datasets, selection, data.name, features.unique, data.merge, data.create) {
  # if there are no datasets, we are done here
  n <- length(datasets);
  if((length(n) <= 0L) || (length(selection) <= 0L)) {
    return(list());
  }

  # pick the matching datasets and create and return a new list of datasets
  result <- lapply(X=seq_along(selection), FUN=function(sel.index) {
    # get the dataset instances belonging to the current selection
    sel <- unlist(lapply(X=selection[[sel.index]], FUN=function(i) datasets[[i]]), recursive=TRUE);

    # merge all the datasets stored in these instances
    sel.data <- data.merge(lapply(X=sel, FUN=function(m) m@data));
    sel.data <- force(sel.data);

    # obtain the features for these models
    sel.features <- features.unique[[sel.index]];
    sel.features <- force(sel.features);

    # compute the new names
    sel.name   <- data.name(unlist(lapply(X=sel, FUN=function(m) m@name), recursive=TRUE),
                        sel.features);
    sel.name   <- force(sel.name);

    # create the new, merged Models instance
    data.create(name=sel.name,
          features=sel.features,
          data=sel.data)
  });

  # create result list
  result <- unname(unlist(result, recursive = TRUE));
  # make sure that all datasets have unique names
  result <- dataset.make.names.unique(result);
  result <- force(result);
  return(result);
}


#' @title Create a Combination or Selection of \code{\link{dataset}s}
#' @description Merge a set of \code{\link{dataset}s} according to a given
#'   selection.
#' @param datasets the list of instances of \code{\link{dataset}s}
#' @param selection a list of vectors of model indexes. \code{\link{dataset}s}
#'   in the same index are joined
#' @param data.name a function used to join names of datasets that now have
#'   identical features, see \code{\link{datasets.joinNames}} for documentation
#' @param data.merge a function for merging the \code{data} lists; by default
#'   \code{unlist(x, recursive=FALSE)}
#' @param data.create a function for creating new datasets, by default
#'   \code{\link{dataset.new}}
#' @return a list of  instances of \code{\link{dataset}} where all names have
#'   been transformed according to \code{data.name}, the features have been selected
#'   according to selection, the data lists have been merged using
#'   \code{data.merge}, and the new instances have been created via
#'   \code{data.create}.
#' @export datasets.merge.by.selection
#' @include dataset.R
#' @include joinNames.R
datasets.merge.by.selection <- function(datasets,
                                        selection=seq_along(datasets),
                                        data.name=dataset.joinNames,
                                        data.merge=.data.merge,
                                        data.create=dataset.new) {
  # if there are no datasets, we are done here
  n <- length(datasets);
  if((length(n) <= 0L) || (length(selection) <= 0L)) {
    return(list());
  }

  # invoke the builder based on the selected models, but first compute the
  # selected features
  return(.datasets.merge.by.selection(models=models,
                                      selection=selection,
                                      data.name=data.name,
                                      features.unique=lapply(X=selection,
   FUN=function(sel) {
     # try to get the feature values for the i'th selection
     # these will be the values that all models have in common
     if(length(sel) == 1L) {
     # only 1 model? return its features as-is
       return(datasets[[sel[[1L]]]]@features);
     }

     # get the list of feature lists
     features <- lapply(X=sel, FUN=function(i) datasets[[i]]@features);

     features.length <- length(features);
     # only one (or zero) lists? odd. return as-is
     if(features.length <= 1L) { return(features); }

     # get the blueprint
     first <- features[[1L]];
     first.length <- length(first);
     if(first.length <= 0L) {
       # no feature? then let's quit and return an empty list
       return(list());
     }

     # iterate over all the features and keep the unique feature only
     names <- ls(first);
     keep  <- rep(TRUE, first.length);
     for(i in seq.int(from=2L, to=features.length, by=1L)) {
       keep <- keep & vapply(X=names, FUN=function(j) identical(first[j], features[[i]][j]),
                             FUN.VALUE=FALSE);
     }
     return(first[keep]);
   }), data.merge=data.merge, data.create=data.create));
}
