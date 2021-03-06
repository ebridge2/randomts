% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/positiveDef.R
\name{positiveDef}
\alias{positiveDef}
\title{A function to randomly sample positive definite matrices.}
\usage{
positiveDef(nf)
}
\arguments{
\item{nf:}{The number of features to sample a positive definite matrix for.}
}
\value{
pos[nf, nf]: A single random positive definite matrix, of dimensions [features, features].
}
\description{
A function to randomly sample positive definite matrices.
}
\examples{
test <- positiveDef(10)
dim(test)
}

