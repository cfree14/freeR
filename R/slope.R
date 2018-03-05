
#' Extract slope of linear regression
#'
#' Extracts the slope of a linear regression.
#'
#' @param lmfit A linear model fit using lm()
#' @return Slope of the linear model fit
#' @export
slope <- function(lmfit){coef(lmfit)[2]}
