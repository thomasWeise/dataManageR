#' @title Select a Dataset by Name
#' @description Return the dataset whose name matches the provided name, or its
#'   index (if {@code getIndex==TRUE)}).
#' @param datasets the list of \code{\link{dataset}} instances
#' @param name the name we are looking for
#' @param getIndex if \code{FALSE} (by default), this function returns the
#'   dataset with the matching name; if \code{TRUE}, just the index of the
#'   dataset is returned.
#' @return the dataset whose name matches \code{name} (\code{getIndex==FALSE})
#'   or its index (\code{getIndex==TRUE}); or \code{NULL} if no dataset matches
#'   the given name
#' @export datasets.select.by.name
datasets.select.by.name <- function(datasets, name, getIndex=FALSE) {
  for(i in seq_along(datasets)) {
    ds <- datasets[[i]];
    if(identical(ds@name, name)) {
      if(getIndex) { return(i); }
      return(ds); }
  }
  return(NULL);
}
