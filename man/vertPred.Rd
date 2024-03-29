% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_prediction_functions.R
\name{vertPred}
\alias{vertPred}
\title{Predict occurrence of a fish species or platypus (total or female) in a set of subcatchments}
\usage{
vertPred(code, vertSamps)
}
\arguments{
\item{code}{an 8-character code as listed in \code{vertSpp}}

\item{vertSamps}{a data.frame of subcatchments (from \code{mwstreams}) each with
associated environmental predictors.}
}
\value{
A vector with length = number of rows in \code{vertSamps},
with each row corresponding to the equivalent row in \code{vertSamps}.
\describe{predicted probabilities of occurrence under conditions specified in \code{sampprPred}}
}
\description{
Predict occurrence of a fish species or platypus (total or female) in a set of subcatchments
}
\details{
The vertSamps table must contain the 9 predictor variables used in the
models. The function \code{collateSampleFP} can
be used to compile such a table. (Note platypus and fish species have different predictors)
}
\seealso{
\code{\link{collateSampleFP}}, \code{\link{vertSpp}}
}
