% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation_functions.R
\name{prob59ToPA}
\alias{prob59ToPA}
\title{Convert a data frame of probabilities of occurrence of 59 families to presence-absence}
\usage{
prob59ToPA(prob59)
}
\arguments{
\item{prob59}{a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
Data for each samppr are 1s for observed presences and 0 for absences}
}
\value{
A data.frame the same structure as prob59, but with probabilities of occurrence
converted to presences (1) or absences (0)
}
\description{
Convert a data frame of probabilities of occurrence of 59 families to presence-absence
}
\details{
Presence is inferred if the probability of occurrence (rounded to 3 decimal places) is
greater than or equal to the \code{pa.threshold} for each family as listed in \code{taxon.classes}
}
\examples{
test59preds <- predCurrNHI59(melbstreambiota::mwstreams[1:4,])
prob59ToPA(test59preds$predCurr)
}
