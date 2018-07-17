#' @title A \code{dataset} is an Object  Identified by a Name and a List of
#'   Features
#' @description Each instance of this class represents a daset identified by a
#'   list of features and a name.
#' @slot name the name of this dataset
#' @slot features a list of name-value pairs denoting the features of the
#'   dataset
#' @slot data the actual data, a list, which will be known under the name
#'   \code{name} and which is annotated with \code{features}
#' @importFrom methods setClass representation
#' @exportClass dataset
dataset <- setClass(
  Class = "dataset",
  representation = representation(name="character",
                                  features="list",
                                  data="list"),
  validity = function(object) {
    # check name: must be exactly one
    if(is.null(object@name) || (length(object@name) != 1L) ||
                               (nchar(object@name) <= 0L)) {
      return("Name must be defined and non-empty.");
    }

    # check the features
    if(is.null(object@features)) {
      return("Features list can be empty, but not NULL.");
    }
    l <- length(object@features);
    if(l > 0L) {
      nams <- names(object@features);
      if(length(nams) != l) {
        return("All features must be uniquely named.");
      }
      for(nam in nams) {
        if(is.null(nam) || (nchar(nam) <= 0L)) {
          return("All feature names must be valid, non-empty strings.");
        }
      }
      for(feature in object@features) {
        if(is.na(feature) || is.null(feature) || (length(feature) != 1L) ||
           (!((is.character(feature) && (nchar(feature) > 0L)) ||
              is.logical(feature) ||
             (is.numeric(feature) && is.finite(feature)) ||
             is.factor(feature)))) {
          return("All features must be non-empty character strings, logical, or finite numbers or factors.");
        }
      }
    }

    if(is.null(object@data) || (!(is.list(object@data)))) {
      return("The data list of the object cannot be null and must be a list.");
    }

    return(TRUE);
  }
)


#' @title Create an Instance of \code{\link{dataset}}
#' @description Instantiate \code{\link{dataset}} using this method.
#' @param name the name of this dataset
#' @param features a list of name-value pairs denoting the features of the dataset
#' @param data the list of with the data, which is named by \code{name} and annotated with \code{features}.
#' @importFrom methods validObject new
#' @export dataset.new
dataset.new <- function(name, features, data) {
  if(length(features) > 1L) {
    features <- features[order(names(features))];
  }
  result <- new("dataset", name=name,
                features=features,
                data=data);
  result <- force(result);
  result@name <- force(result@name);
  result@features <- force(result@features);
  result@data <- force(result@data);
  result <- force(result);
  validObject(result);
  return(result);
}
