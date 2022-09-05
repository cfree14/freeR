
#' Returns a list of sorted unique values
#'
#' Returns a list of sorted unique values.
#'
#' @param x A character vector
#' @return A sorted and unique character vector
#' @export
uniq <- function(x){

  x1 <- sort(unique(x))
  return(x1)

}
