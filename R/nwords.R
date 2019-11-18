
#' Number of words
#'
#' Counts and returns the number of words in each element of a character vector.
#'
#' @return Number of words in a string
#' @examples
#' # Number of words
#' phrases <- c("Words", "Two words", "This is four words.")
#' nwords(phrases)
#' @export
nwords <- function(x){
  nwords <- sapply(strsplit(x, " "), length)
  return(nwords)
}
