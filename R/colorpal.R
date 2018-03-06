
#' Create expanded color palette
#'
#' Creates an expanded color palette from an existing color palette by interpolating new colors.
#'
#' @param pal A vector of colors
#' @param n Number of colors to be interpolated
#' @return An expanded vector of colors
#' @examples
#' # Make 15 color palette from 11 color red-blue palette
#' library(RColorBrewer)
#' colorpal(brewer.pal(11, "RdBu"), 15)
#' @export
colorpal <- function(pal, n){colorRampPalette(pal)(n)}
