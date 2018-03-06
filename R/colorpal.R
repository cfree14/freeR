
#' Create expanded color palette
#'
#' Creates an expanded color palette from an existing color palette by interpolating new colors.
#'
#' @param pal A vector of colors
#' @param n Number of colors to be interpolated
#' @return An expanded vector of colors
#' @export
colorpal <- function(pal, n){colorRampPalette(pal)(n)}
