#' @title Select a Datasets by Name
#' @description Return the datasets whose names matches the provided
#'   \code{include} names while being different from the \code{exclude} names.
#'   If {@code getIndex==TRUE)}, return their indices instead.
#' @param datasets the list of \code{\link{dataset}} instances
#' @param include the names we are looking for: if this is not \code{NULL}, then
#'   only datasets with a name in this list will be returned
#' @param exclude the names we do not want: if this is not \code{NULL}, then
#'   only datasets whose name does not appear in this list will be permitted
#' @param getIndex if \code{FALSE} (by default), this function returns the
#'   datasets with the matching name; if \code{TRUE}, just the indexes of the
#'   datasets are returned.
#' @return the dataset whose names match \code{name} (\code{getIndex==FALSE}) or
#'   their indexes (\code{getIndex==TRUE})
#' @export datasets.select.by.name
datasets.select.by.name <- function(datasets, include=NULL, exclude=list(), getIndex=FALSE) {

  # create the inclusion criterion
  if(is.null(include)) {
    # no inclusion criterion
    if(is.null(exclude) || (length(exclude) <= 0L)) {
      # no criteria, return as-is
      if(getIndex) { return(seq_along(datasets)); }
      return(datasets);
    } else {
      # only exclusion criteria
      FUN <- function(ds) !(ds@name %in% exclude);
    }
  } else {
    # we have inclusion criteria
    if((!is.null(exclude)) && (length(exclude) > 0L)) {
      # if we both have limites on inclusion of strings and exclusion, then we
      # first collapse them into only inclusion limits
      include <- include[vapply(X=include, FUN=function(i) (!(i %in% exclude)), FUN.VALUE=FALSE)];
    }

    if(length(include) <= 0L) {
      # no inclusion
      if(getIndex) { return(integer(0L)); }
      return(list());
    }

    # create the criterion
    FUN <- function(ds) (ds@name %in% include);
  }

  # enforce criterion
  FUN <- force(FUN);

  # compute inclusion
  sel <- vapply(X=datasets, FUN=FUN, FUN.VALUE = FALSE);

  # return the result
  if(getIndex) {
    return(seq_along(datasets)[sel]);
  }
  return(datasets[sel]);
}
