#' A function to randomly sample positive definite matrices.
#'
#' @param nf: The number of features to sample a positive definite matrix for.
#' @return pos[nf, nf]: A single random positive definite matrix, of dimensions [features, features].
#' @examples
#' test <- positiveDef(10)
#' dim(test)
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

positiveDef <- function(nf) {
  positive_def = rnorm(nf)
  positive_def = positive_def %*% t(positive_def)
  while (min(Re(eigen(positive_def)$values)) < 0) {
    positive_def = positive_def + diag(nf)
  }
  return(positive_def)
}
