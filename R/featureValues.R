# check and try to convert a numeric vector to an integer vector
.check.numeric <- function(vec) {
  na <- !is.na(vec);
  if(any(na & (!is.finite(vec)))) { return(vec); }
  suppressWarnings({
    test <- as.integer(vec);
    if(any((!is.na(test)) != na)) { return(vec); }
    if(any(na & !is.finite(test))) { return(vec); }

    check <- abs(test[na] - vec[na]);
    if(any(is.na(check) | (!is.finite(check)))) { return(vec); }
    if(all(check < .Machine$double.eps, na.rm = TRUE)) {
      return(test);
    }
    return(vec);
  });
}


#' @title Get a Vector with the Values of one Feature for all
#'   \code{\link{dataset}s}
#' @description For each \code{\link{dataset}}, get the value of the specified
#'   feature. If the feature is undefined, its value is \code{NA}.
#' @param datasets the list of \code{\link{dataset}s} from where we want to
#'   extract the feature names
#' @param featureName the name of the feature
#' @return the vector with the values of this feature
#' @export datasets.feature.values
#' @include dataset.R
datasets.feature.values <- function(datasets, featureName) {
  # get the values
  values <- lapply(X=datasets, FUN=function(ds) {
    value <- ds@features[[featureName]];
    if(is.null(value)) { return(NA); }
    return(value);
  });

  # get the unique types
  types <- unique(unlist(lapply(X=values, FUN=function(value) {
     if(is.na(value)) { return(NULL); }
     return(typeof(value)); }), recursive=TRUE));

  tl <- length(types);
  if(tl <= 0L) {
    # feature is nowhere defined
    return(rep(NA, length(datasets)));
  }

  if(tl <= 1L) {
    # if there is only a single type, then we can use this type
    if(types[[1L]] == "character") {
      res <- vapply(X=values, FUN=as.character, FUN.VALUE=NA_character_);
    } else {
      res <- as.vector(x=values, mode=types[[1L]]);
    }

    if(identical(types[[1L]], "double") ||
       identical(types[[1L]], "numeric")) {
      # if the vector would be numerical, check if we can return
      # it as integer vector
      return(.check.numeric(res));
    }
    # otherwise, return as is
    return(res);
  }

  if(tl == 2L) {
    # ok, we have two different types: they could be numerical
    types <- sort(types);
    if(identical(types, c("integer", "numeric")) ||
       identical(types, c("double", "integer"))) {
      # both are numerical, treat as numerical vector - but check if it can be
      # converted to an integer vector
      return(.check.numeric(as.vector(x=values, mode="numeric")));
    }
  }

  # more than two types or at least two types and no numerical type: read as
  # character vector
  return(vapply(X=values, FUN=as.character, FUN.VALUE=NA_character_));
}
