
#' Round and format to specified decimal place
#'
#' Rounds and formats a number to specified decimal place.
#'
#' @param x Number(s)
#' @param decimals Number of decimals
#' @return Formatted number(s) in character format
#' @examples
#' roundf(0.345957, 2)
#' @export
roundf <- function(x, decimals){format(round(x, decimals), nsmall=decimals)}
