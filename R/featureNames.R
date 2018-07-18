#' @title Get the Names of All Features of All Datasets
#' @description Get the names of all features of all \code{\link{dataset}s}
#' @param datasets the list of \code{\link{dataset}s} from where we want to
#'   extract the feature names
#' @return the feature names (alphabetically sorted)
#' @export datasets.feature.names
#' @include dataset.R
datasets.feature.names <- function(datasets)
   sort(unique(unname(unlist(lapply(X=datasets,
                                    FUN=function(ds) names(ds@features)), recursive = TRUE))))
