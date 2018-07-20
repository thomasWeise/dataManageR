#' @title Transform a List of Datasets to a List with Named Values
#' @description A list of \code{\link{dataset}}s is translated to another list,
#'   where each element is, in turn, a list. Each such list belongs to one group
#'   and, as first element, has the group name. This name is followed by other
#'   features computed by the functions in list \code{transformers} and named
#'   accordingly.
#' @param data the list of \code{\link{dataset}}s
#' @param groups the group names
#' @param transformers a list of functions whose names will be the names of the
#'   new features and the new feature values are computed by these functions
#' @return a list of lists, where each item belongs to exactly one group
#' @export datasets.as.lists
datasets.as.lists <- function(data,
                              groups,
                              transformers) {
  stopifnot(length(groups) == length(data));

  return(lapply(X=unique(groups),
                FUN=function(group) {
                  source             <- data[groups == group];
                  transformers$group <- function(x) group;
                  return(lapply(X=transformers, FUN=function(x) x(source)));
                }));
}
