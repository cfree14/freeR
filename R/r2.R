
#' Extract r2 of linear regression
#'
#' Extracts the r2 of a linear regression.
#'
#' @param lmfit A linear model fit using lm()
#' @return r2 of the linear model fit
#' @export
r2 <- function(lmfit){summary(lmfit)$r.squared}
