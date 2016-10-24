#' A function to randomly sample a population of timeseries from a user-defined model type.
#'
#' @param numsubs: the number of subjects.
#' @param numsessions: the number of sessions per subject.
#' @param nt: the number of timesteps.
#' @param nf: the number of features.
#' @param options: the model to sample. Options are:
#' \itemize{
#'    \item 'gaussian': a gaussian random model.
#' }
#'
#' @param noise: a parameter whether to add zero-mean noise. Provide the sigma (noise=0 is to add no noise).
#' @param simplify: a parameter specifying whether to simplify the output. Options are FALSE and 'array'.
#' @return ts: the output population timeseries. Can be formatted as:
#' \itemize{
#'    \item if simplify=FALSE:
#'    \itemize{
#'      \item ts[[sub]][[session]]$signal: the signal for a particular subject. Dimensions are [timesteps, features].
#'      \item ts[[sub]][[session]]$model: the model used for a particular subject.
#'    }
#'    \item if simplify='array':
#'    \itemize{
#'      \item ts[timestep, feature, subject, session]
#'    }
#' }
#' @examples
#' ts <- sample_random_population(4, 2, 100, 1, option='gaussian')

sample_random_population <- function(numsubs, numsessions,
                                     nt, nf, option='gaussian',
                                     noise=FALSE, simplify=FALSE) {
  ts <- sapply(1:numsubs, function(sub) {
    model <- random_model(nf, option=option)
    tssub <- sapply(1:numsessions, function(session) {
      sample_random_timeseries(nt, nf, model=model, noise=noise, simplify=simplify)
    }, USE.NAMES=TRUE, simplify=simplify)
  }, USE.NAMES=TRUE, simplify=simplify)

  return(ts)
}
