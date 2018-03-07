
#' Clear workspace
#'
#' Clears workspace (i.e., removes all objects from environment)
#'
#' @examples
#' clear()
#' @export
clear <- function(){rm(list = ls())}
