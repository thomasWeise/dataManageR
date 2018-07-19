.name.separator <- "/"
.name.undefined <- "unnamed"

#' @title The Default Namer for Datasets
#' @description This is the default method to create names from name components.
#' @param components the name components
#' @return the dataset name representing the \code{components}
#' @export datasets.names.namer
datasets.names.namer <- function(components) {
  if(is.null(components) || (length(components) <= 0L)) {
    return(.name.undefined);
  }
  components <- trimws(components);
  components <- components[nchar(components) > 0];
  if(is.null(components) || (length(components) <= 0L)) {
    return(.name.undefined);
  }
  paste(components, sep=.name.separator, collapse=.name.separator)
}
