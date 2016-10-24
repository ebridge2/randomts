#' A function to create randomized parameters for models that are of particular
#' importance when considering potential simulated timeseries.
#'
#' @param nf: the number of features.
#' @param option: the model to sample from. Options are 'gaussian'.
#' @return model: a model to provide parameters for the particular distribution chosen.
#' \itemize{
#'  \item gaussian model:
#'    \itemize{
#'      \item model$type = 'gaussian'
#'      \item model$mean[nf]: a mean vector for the timeseries to be generated.
#'      \item model$cov[nf, nf]: a covariance matrix for the timeseries.
#'    }
#' }
#' @examples
#' test <- random_model(option='gaussian')

random_model <- function(nf, option='gaussian') {
  model <- list()
  if (isTRUE(all.equal(option, 'gaussian'))) {
    model$type <- 'gaussian'
    model$mean <- rnorm(nf)
    model$cov <- positiveDef(nf)
  }
  return(model)
}
