#' @include dataset.R
#' @include featuresDefaultLoader.R
#' @include makeNamesUnique.R

#' @title Load the a Sequence of \code{\link{dataset}}s
#' @description Load all the \code{\link{dataset}}s from a folder structure.
#' @param path the source folder
#' @param selector the selector for the files
#' @param dataLoader a \code{function(file)} invoked for every single file
#'   selected by the selector which returns one data item to be attached to the
#'   \code{data} member of a dataset
#' @param featuresFolder the folder where to look for feature information,
#'   \code{NULL} if no feature information is needed
#' @param featuresLoader a function accepting two parameters,
#'   \code{featureFolder} and \code{components} and returning a named list of
#'   features, or \code{NULL} if no features are needed. see
#'   \code{\link{datasets.feature.load.default}} for documentation
#' @param nameProcessor a function which receives a relativized path to the
#'   folder with the current model(s) and returns a name for the models
#' @param check.directory a function which can choose if a directory should be
#'   followed or not
#' @param cores the number of cores to be used for loading
#' @param logging should progress information be printed: either \code{TRUE} for
#'   printing to the console via \code{\link{print}}, \code{FALSE} for no
#'   logging, or a path to a file receiving logging information
#' @return a list of \code{\link{dataset}s} instances
#' @importFrom utilizeR path.batchApply path.extensionRegExp makeLogger path.relativize
#' @importFrom utils read.csv
#' @export datasets.batchLoad
datasets.batchLoad <- function(path=getwd(),
                               selector=path.extensionRegExp(extensions="csv"),
                               dataLoader=function(file) read.csv(file),
                               featuresFolder=file.path(path, "../features"),
                               featuresLoader=datasets.feature.load.default,
                               nameProcessor=identity,
                               check.directory=NULL,
                               cores=1L,
                               logging=(cores <= 1L)) {

  path <- force(path);
  path <- normalizePath(path);
  path <- force(path);

  # force and canonicalize the features loading process
  featuresFolder <- force(featuresFolder);
  featuresLoader <- force(featuresLoader);
  if(is.null(featuresLoader)) {
    featuresFolder <- NULL;
  } else {
    if(is.null(featuresFolder)) {
      featuresLoader <- NULL;
    } else {
      featuresFolder <- normalizePath(featuresFolder, mustWork=TRUE);
    }
  }

  # first we make the logger function useable
  logging <- makeLogger(logging, cores);
  logging <- force(logging);
  if(!is.null(logging)) {
    ftxt <- if(is.null(featuresFolder)) " without features."
            else paste(" with features from folder ", featuresFolder, ".",
                       sep="", collapse="")
    logging("now loading datasets from ", path, ftxt);
  }

  nameProcessor   <- force(nameProcessor);
  check.directory <- force(check.directory);
  cores           <- force(cores);
  dataLoader      <- force(dataLoader);

  # build the internal loader function
  loader <- function(root, paths) {
    root           <- force(root);
    paths          <- force(paths);
    nameProcessor  <- force(nameProcessor);
    featuresFolder <- force(featuresFolder);
    featuresLoader <- force(featuresLoader);
    dataLoader     <- force(dataLoader);

    # we only load data from files of non-zero size
    paths <- paths[file.exists(paths) & (file.size(paths) > 0L)];
    if(length(paths) <= 0L) { return(NULL); }

    # first, for each path load a data record
    results <- unname(lapply(X=paths, FUN=dataLoader));

    # if there are no results for any of the files, let's simply stop here
    if(is.null(results) || (length(results) <= 0L)) {
      return(NULL);
    }

    # first we extract the name part from the directory
    name <- path.relativize(dirname(paths[1L]), root);

    # now: de we need to apply the features loader
    if(!(is.null(featuresFolder) || is.null(featuresLoader))) {
      # yes, so first split the name
      components <- unname(unlist(strsplit(name, "/"), recursive = TRUE));
      features   <- featuresLoader(featuresFolder, components);
    } else {
      features <- list();
    }

    # ok, we have results, so now we need to create the name
    name <- nameProcessor(name);
    if(is.null(names) || (nchar(name) <= 0L)) {
      names <- "unnamed"; # if there are no names, use "unnamed"
    }

    # create and return the results record
    result <- dataset.new(name=name, features=features, data=results);
    result <- force(result);
    return(result);
  }
  loader <- force(loader);

  # we now have a loader function ready
  # assign the loader to the regular expression for processors
  file.all <- new.env();
  assign(x=selector, value=loader, envir=file.all);
  file.all <- force(file.all);

  # execute the batch process
  results <- path.batchApply(path=path, file.single=NULL, file.in.folder=file.all,
                             cores=cores, check.directory=check.directory,
                             logging=logging);

  # consolidate the result
  results <- unname(unlist(results, recursive = TRUE));

  if(!(is.null(logging))) {
    # print the status message
    logging("finished loading ", length(results), "datasets.");
  }

  # canonicalize the result
  if(is.null(results) || (length(results) <= 0L)) {
    return(NULL);
  }

  # make sure that all models have unique names
  results <- datasets.names.make.unique(results);
  results <- force(results);
  return(results);
}
