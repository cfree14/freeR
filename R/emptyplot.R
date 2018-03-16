
#' Creates an empty plot
#'
#' Creates an empty plot with both axis extending from 0 to 10.
#'
#' @examples
#' emptyplot()
#' @export
emptyplot <- function(){plot(0:10, 0:10, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")}
