
#' Identify duplicated items
#'
#' Returns vector of duplicated items.
#'
#' @param x A vector
#' @return A vector of unique duplicated items
#' @examples
#' # Get taxonomic info
#' vec <- c("Gadus morhua", "Gadus morhua", "Centropristis striata")
#' which_duplicated(vec)
#' @export
which_duplicated <- function(x) {
  y <- sort(unique(x[duplicated(x)]))
  return(y)
}





