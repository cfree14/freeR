
#' Change text to sentence case
#'
#' Changes string vector to sentence case.
#'
#' @param x A character vector in any case
#' @return A character vector in sentence case
#' @examples
#' # Convert to sentence case
#' sentcase(x=c("Gadus Morhua", "gadus morhua", "GADUS morhua"))
#' @export
sentcase <- function(x){

  x1 <- trimws(tolower(x))
  x2 <- paste0(toupper(substr(x1, 1, 1)), substr(x1, 2, nchar(x1)))

}
