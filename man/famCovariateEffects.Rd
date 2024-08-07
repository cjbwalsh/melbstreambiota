% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lumar_diagnostic_functions.R
\name{famCovariateEffects}
\alias{famCovariateEffects}
\title{Find the relative contribution of families to differences in LUMaR associated with a covariate}
\usage{
famCovariateEffects(obs.table, exp.table, covariate)
}
\arguments{
\item{obs.table}{a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
Data for each samppr are 1s for observed presences and 0 for absences}

\item{exp.table}{a data.frame with the same structure as obs.table, but filled with probabilities of occurrence
as predicted by the 59 distribution models described by Walsh (in prep) under no human impact}

\item{covariate}{a vector of covariate values corresponding to samppr used as a contrast in LUMaR scores}
}
\value{
A data frame with rows corresponding to the families found or expected in the sampprs (<= 59) and 6 columns:
\describe{
  \item{fam}{the \code{bugcode}s matched by \code{taxon.classes$fam}}
  \item{unexp}{0 = families whose presence contributed to differences in LUMaR by their sensitivity grade;
  1 = families whose presence contributed by their unexpected grade.}
  \item{coef}{the coefficient of a beta regression of the weighted occurrences of each families against the covariate}
  \item{lwr}{the 10th percentile confidence interval of coef}
  \item{upr}{the 90th percentile confidence interval of coef}
  \item{sig}{'significance': TRUE if the range between lwr and upr includes zero}
  }
}
\description{
Find the relative contribution of families to differences in LUMaR associated with a covariate
}
\examples{
sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
                                                package = "melbstreambiota", mustWork = TRUE),
                                    sheet = 1))
bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
                                                package = "melbstreambiota", mustWork = TRUE),
                                    sheet = 2))
sampprs <- collateBugSamppr(sampprs)
expTable <- predCurrNHI59(sampprs)$predNHI
obsTable <- collateObsTable(bugData, sampprs)
sampprs$stream <- substr(sampprs$samppr,3,5)
famCovariateEffects(obsTable, expTable, sampprs$stream)
}
\seealso{
\code{\link{collateBugSamppr}}, \code{\link{predCurrNHI59}}, \code{\link{collateObsTable}}
}
