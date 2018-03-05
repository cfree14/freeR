
#' Extract p-value of linear regression
#'
#' Extracts the p-value of a linear regression.
#'
#' @param lmfit A linear model fit using lm()
#' @return p-value of the linear model fit
#' @export
pval <- function(lmfit){anova(lmfit)$'Pr(>F)'[1]}
