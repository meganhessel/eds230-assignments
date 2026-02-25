#'  Logistic forest growth derivative
#' @param time time since start
#' @param C forest size as a unit of carbon 
#' @param parms - as list with three values, r, g, K
#' @param r exponential growth rate
#' @param g linear growth rate once canopy has been reached
#' @param K carrying capacity
#' @return derivative of forest size with time


forest_growth_func <- function(time, C, parms, thresh = 50) {
  
  if(C < thresh) {
    dC <- parms$r * C
  }else{
    dC <- parms$g * (1 - C / parms$K)
  }
  
  return(list(dC))
}
