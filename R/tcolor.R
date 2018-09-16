
#' Make transparent color
#'
#' Makes a color represented by a word ("green") or hex code ("#4FFF33) transparent.
#'
#' @param color A color (e.g., "green", "#4FFF33)
#' @param alpha The transparency level (0-1; 0=invisible, 1=opaque)
#' @return A color with transparency
#' @examples
#' # Plot using transparent red
#' plot(1:10, 1:10, col=tcolor("red", alpha=0.4))
#' @export
tcolor <- function(color, alpha){rgb(t(col2rgb(color))/255, alpha=alpha)}
