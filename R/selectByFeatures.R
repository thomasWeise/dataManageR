#' @title Select a Dataset by Features
#' @description Return the datasets whose features match the provided features,
#'   or their indexes (if {@code getIndex==TRUE)}). This function returns a list
#'   of only the datasets whose features match to all the constraints given in
#'   \code{features}. Each element of \code{features} must be named and the name
#'   refers to a feature in the dataset. The values of the features can be
#'   provided as list or vector of allowed values.
#' @param datasets the list of \code{\link{dataset}} instances
#' @param features a list with named elements, we only return datasets where all features match
#' @param getIndex if \code{FALSE} (by default), this function returns the
#'   datasets with the matching features; if \code{TRUE}, just the indexes of the
#'   datasets is returned.
#' @return the datasets whose features match \code{features} (\code{getIndex==FALSE})
#'   or their indexes (\code{getIndex==TRUE}); or an empty list if no dataset matches
#'   the given features
#' @export datasets.select.by.features
datasets.select.by.features <- function(datasets, features, getIndex=FALSE) {

  features.length <- length(features);
  features.names  <- names(features);
  seq.i           <- seq_along(datasets);

  # if there is nothing to select from, just return everything as is
  if(is.null(features.length) || (features.length <= 0L) || is.null(features.names)) {
    if(getIndex) { return(seq.i); }
    return(datasets);
  }

  seq.j <- seq_along(features);

  #ok, do the selection: find out which datasets fulfill the criterio
  selected <- vapply(X=datasets,
                     FUN=function(ds) {
                            for(j in seq.j) {
                              f <- ds@features[features.names[j]];
                              if(is.null(f)) { return(FALSE); }
                              if(!(f %in% features[[j]])) {
                                return(FALSE);
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
