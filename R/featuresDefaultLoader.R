#' @include featuresTxtLoader.R

#' @title The Default Loader for Features
#' @description The default loader for the \code{\link{dataset}} features is the
#'   \code{\link{features.txtLoader}}
#' @param featureFolder the folder with the features
#' @param components the components
#' @param values.eval the evaluated values
#' @return a named list, where the names are the feature names and the values
#'   the corresponding feature values
#' @export features.defaultLoader
features.defaultLoader <-  features.txtLoader
