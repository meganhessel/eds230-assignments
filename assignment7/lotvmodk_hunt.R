#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred number of predictor
#' @param min_prey Number of prey that is necessary for hunting (default = 50 - shop hunting when prey gets to 50)
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#'  \emph{h_prey} hunting rate on the prey;
#'  \emph{pmort} mortality rate of predictor population;
#' @examples
#' COME BACK AND ADD EXAMPLES
#'
#' @return  pred_prey_fun returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#' }

lotvmodk_hunt <- function(t, pop, pars, min_prey = 50) {
  with(as.list(c(pars, pop)), {
    
    # Hunt only when prey populations are above the thresholds (min_prey)
    h_prey <- ifelse(h_prey * prey > min_prey, h_prey, 0)

    # Scaling h_prey and h_pred with the population size -> when prey = 0, hunting = 0
    dprey <- (rprey * (1 - prey / K) * prey) - (alpha * prey * pred) - h_prey
    dpred <- (eff * alpha * prey * pred) - (pmort * pred)
    
    return(list(c(dprey, dpred)))
  })
}
