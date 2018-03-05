
#' Check completeness of dataframe
#'
#' Checks the completeness of columns in a dataframe.
#'
#' @param data A dataframe
#' @return A vector showing the number of NA values in each column of a dataframe
#' @export
complete <- function(data){apply(data, 2, function(x) sum(is.na(x)))}
