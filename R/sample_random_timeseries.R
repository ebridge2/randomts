#' A function to randomly sample a single random timeseries
#'
#' @param nt: the number of timesteps.
#' @param nf: the number of features.
#' @param options: the model to sample. Options are:
#' \itemize{
#'    \item 'gaussian': a gaussian random model.
#'    \item if model is specified, this option is irrelevant.
#' }
#' @param model: a model to provide parameters for the particular distribution chosen.
#' \itemize{
#'  \item gaussian model:
#'    \itemize{
#'      \item model$type = 'gaussian'
#'      \item model$mean[nf]: a mean vector for the timeseries to be generated.
#'      \item model$cov[nf, nf]: a covariance matrix for the timeseries.
#'    }
#' }
#' @param noise: a parameter whether to add zero-mean noise. Provide the sigma (noise=FALSE is to add no noise).
#' @param simplify: A parameter whether to simplify the output.
#' \itemize{
#'  \item if simplify=='TRUE'array': returns only the signal.
#'  \item if simplify==FALSE: returns a list of the signal and the model.
#' }
#' @return signal[nt, nf]: A random timeseries.
#' @return model: the model chosen. Only returned if simplify=FALSE.
#' @examples
#' test <- sample_random_timeseries(100, 10, options='gaussian')
#' @export

sample_random_timeseries <- function(nt, nf, option='gaussian', model=NULL, noise=FALSE, simplify=FALSE) {
  require('MASS')
  if (is.null(model)) {
    model <- random_model(option=option)
  } else if(is.null(model$type)) {
    stop(paste('Your model is not correct. Type is:', model$type))
  }

  if (isTRUE(all.equal(model$type, 'gaussian'))) {
    signal <- mvrnorm(n=nt, mu=model$mean, Sigma=model$cov)
  } else {
    stop('The model you have entered is not supported.')
  }
  if (isTRUE(all.equal(noise, as.numeric(noise)))) {
    noise <- sapply(1:nf, function(x) mvrnorm(n=nt, mu=0, Sigma=noise), simplify=TRUE)
    signal <- signal + noise
  }
  if (isTRUE(all.equal(simplify, 'array'))) {
    return(signal)
  } else {
    return(list(signal=signal, model=model))
  }
}
