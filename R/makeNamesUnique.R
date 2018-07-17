#' @title Receive a List of \code{\link{dataset}s} Instances and Make their
#'   Names Unique
#' @description Receive a list of instances of class \code{\link{dataset}s} and
#'   make sure that all of their names are unique.
#' @param datasets the list of \code{\link{dataset}s} instances
#' @return a list of \code{\link{dataset}s} instances where no name appears
#'   twice
#' @include names.R
#' @include dataset.R
#' @export datasets.make.names.unique
datasets.make.names.unique <- function(datasets) {
  datasets.length <- length(datasets);
  if(datasets.length > 1L) {
    names <- datasets.names(datasets);
    count <- vapply(X=names, FUN=function(name) sum(names==name), FUN.VALUE=0L);
    index <- rep(x=1L, times=datasets.length);
    if(max(count) > 1L) {
      first <- vapply(X=names, FUN=function(name) which(names==name)[1L], FUN.VALUE=1L);
      for(i in seq_along(datasets)) {
        if(count[i] > 1L) {
          j <- first[i];
          datasets[[i]]@name <- paste(datasets[[i]]@name, ".", index[[j]],
                                      sep="", collapse="");
          datasets[[i]]@name <- force(datasets[[i]]@name);
          datasets[[i]] <- force(datasets[[i]]);
          index[[j]] <- index[[j]] + 1L;
        }
      }
      return(datasets);
    }
  }
  return(datasets);
}
