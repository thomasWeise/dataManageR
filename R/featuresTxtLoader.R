#' @title The Text-based Loader for Features
#' @description The text-based loader for the \code{\link{dataset}} features
#'   searches the \code{featureFolder} for all text files whose names match to
#'   the supplied path \code{components}. Each such file is loaded as a
#'   Java-style properties file with \code{name=value} type data and
#'   \code{#}-based comments. The resulting list contains all the corresponding
#'   names and values. Values can be parsed if \code{value.eval} is set to
#'   \code{TRUE}.
#' @param featureFolder the folder with the features
#' @param components the components
#' @param values.eval the evaluated values
#' @return a named list, where the names are the feature names and the values
#'   the corresponding feature values
#' @export datasets.feature.load.text
#' @importFrom utils read.table
datasets.feature.load.text <- function(featureFolder, components, values.eval=TRUE) {
  # only if the features folder exists...
  if(dir.exists(featureFolder)) {
    # ...we iterate over all components
    result <- (unlist(lapply(X=components,
                             FUN=function(component) {
                      # generate the corresponding file name for the component
                      file <- file.path(featureFolder, paste(trimws(component),
                                        ".txt", sep="", collapse=""));
                      # and if that file actually exists
                      if(file.exists(file) && (file.size(file) > 0L)) {
                        # we load the file as string matrix
                        data <- as.matrix(unname(read.table(file=file, header=FALSE,
                                                            sep="=", row.names=NULL,
                                                            comment.char="#",
                                                            colClasses = "character",
                                                            strip.white=TRUE,
                                                            blank.lines.skip = TRUE,
                                                            flush=TRUE,
                                                            col.names=c(1L, 2L))));
                        if(values.eval) {
                          ret <- lapply(X=data[,2L],
                                        FUN=function(value) {
                                          try({
                                            parsed <- parse(text=value);
                                            envx <- new.env();
                                            result <- eval(parsed, envir=envx, enclos=envx);
                                            if(is.numeric(result) && is.finite(result)) {
                                              suppressWarnings({
                                                test <- as.integer(result);
                                                if(test == result) {
                                                  return(test);
                                                }
                                              });
                                            }
                                            return(result);
                                            }, silent=TRUE);
                                          return(value);
                                        });
                        } else {
                          ret <- data[,2L];
                        }
                        names(ret) <- data[,1L]
                        return(ret);
                      }
                    }), recursive=FALSE, use.names=TRUE));

    if(!(is.null(result))) {
      nr <- names(result);
      len <- length(unique(nr));
      if(len != length(nr)) {
        stop("All names must be unique.");
      }

      result <- result[order(nr)];
      return(result);
    }
  }
  return(list());
}
