#' @title Select a Dataset by Features
#' @description Return the datasets whose features match the provided feature
#'   restrctions, or their indexes (if {@code getIndex==TRUE)}). This function
#'   returns a list of only the datasets whose features match to all the
#'   constraints given in \code{feature.include} and \code{exclude}. Each
#'   element of \code{include} and \code{exclude} must be named and the name
#'   refers to a feature in the dataset. The values of the features can be
#'   provided as list or vector of allowed values. A dataset is only returned if
#'   \itemize{ \item For each tuple \code{a=b} in \code{include}, the feature of
#'   name \code{a} of the dataset has one value from \code{b}. \code{b} can
#'   either be a single value, a vector, or a list. \item For each tuple
#'   \code{c=d} in \code{exclude}, the feature of name \code{c} of the dataset
#'   has one value from \code{d}. \code{d} can either be a single value, a
#'   vector, or a list. }
#' @param datasets the list of \code{\link{dataset}} instances
#' @param include a list with named elements, we only return datasets where all
#'   features match
#' @param exclude a list with named elements, we only return datasets where all
#'   features match
#' @param getIndex if \code{FALSE} (by default), this function returns the
#'   datasets with the matching features; if \code{TRUE}, just the indexes of
#'   the datasets is returned.
#' @return the datasets whose features match \code{features}
#'   (\code{getIndex==FALSE}) or their indexes (\code{getIndex==TRUE}); or an
#'   empty list if no dataset matches the given features
#' @export datasets.select.by.features
datasets.select.by.features <- function(datasets, include=list(), exclude=list(), getIndex=FALSE) {

  if(is.null(include)) {
    stop("Features to include cannot be null.")
  }
  if(is.null(exclude)) {
    stop("Features to exclude cannot be null.")
  }

  include.length <- length(include);
  include.names  <- names(include);
  exclude.length <- length(exclude);
  exclude.names  <- names(exclude);
  seq.i          <- seq_along(datasets);

  # if there is nothing to select from, just return everything as is
  if((include.length <= 0L)  && (exclude.length <= 0L)) {
    if(getIndex) { return(seq.i); }
    return(datasets);
  }

  stopifnot(exclude.length == length(exclude.names),
            include.length == length(include.names))

  seq.include.j <- seq_along(include);
  seq.exclude.j <- seq_along(exclude);

  #ok, do the selection: find out which datasets fulfill the criterio
  selected <- vapply(X=datasets,
                     FUN=function(ds) {
                       # first: check that all include features are there and
                       # match
                            for(j in seq.include.j) {
                              f <- ds@features[include.names[j]];
                              if(is.null(f)) { return(FALSE); }
                              if(!(f %in% include[[j]])) {
                                return(FALSE);
                              }
                            }
                       # second: check that all exclude features are eithre not
                       # there or do not match
                            for(j in seq.exclude.j) {
                              f <- ds@features[exclude.names[j]];
                              if(!is.null(f)) {
                                if(f %in% exclude[[j]]) {
                                  return(FALSE);
                                }
                              }
                            }
                          return(TRUE);
                       }, FALSE);

  # now let's check the selection
  count <- sum(selected);
  # nothing selected
  if(count <= 0L) {
    if(getIndex) { return(integer(0)); }
    return(list());
  }

  #  all selected
  if(count >= length(datasets)) {
    if(getIndex) { return(seq.i); }
    return(datasets);
  }

  # a proper subset is selected
  if(getIndex) { return(seq.i[selected]); }
  return(datasets[selected]);
}
