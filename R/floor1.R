
#' Round to floor of specified level
#'
#' Lowers a number to the nearest 0.1, 0.5, 1, 5, 10, etc.
#'
#' @param x A number
#' @param level The "level" to round the number to
#' @return A rounded number
#' @examples
#' # Lower 0.34 to nearest 0.25
#' floor1(0.34, level=0.25)
#' #' Lower 35.6 to nearest 30
#' floor1(35.6, level=30)
#' @export
floor1 <- function(x, level){floor(x/level)*level}
