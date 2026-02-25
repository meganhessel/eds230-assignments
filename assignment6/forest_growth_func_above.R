#'  Logistic population growth derivative
#' @param time time since start
#' @param P population
#' @param parms - as list with two values, r, K
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @return derivative of population with time

forest_growth_above <- function(Time, P, parms) {
  dC <- parms$g * (1 - C / parms$K)
  return(list(dC))
}

dC/dt = g ∗ (1 − C/K)