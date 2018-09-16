
#' Set rounded axis limits
#'
#' Set rounded axis limits given data and a specified rounding level.
#'
#' @param x A numeric vector of data
#' @param level The "level" to round the lower and upper limits to
#' @return A numeric vector with the lower and upper limit
#' @examples
#' x <- seq(3,95,1)
#' axlim(x, 5)
#' axlim(x, 10)
#' axlim(x, 30)
#' @export
axlim <- function(x, level){
  lo <- freeR::floor1(min(x, na.rm=T), level)
  hi <- freeR::ceiling1(max(x, na.rm=T), level)
  lim <- c(lo, hi)
  return(lim)
}
