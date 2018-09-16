
#' Round to ceiling of specified level
#'
#' Raises a number to the nearest 0.1, 0.5, 1, 5, 10, etc.
#'
#' @param x A number
#' @param level The "level" to round the number to
#' @return A rounded number
#' @examples
#' # Raise 0.34 to nearest 0.5
#' ceiling1(0.34, level=0.5)
#' #' Raise 35.6 to nearest 60
#' ceiling1(35.6, level=50)
#' @export
ceiling1 <- function(x, level){ceiling(x/level)*level}
