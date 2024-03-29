% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_prediction_functions.R
\name{bugModPredSIGNAL}
\alias{bugModPredSIGNAL}
\title{Predict SIGNAL and SIGNAL2 scores for a set of sample-pairs}
\usage{
bugModPredSIGNAL(sampprPreds)
}
\arguments{
\item{sampprPreds}{a data.frame of sample-pair codes (samppr) each with
associated environmental predictors. If there is no field called samppr,
then the field subc if used.}
}
\value{
{A data frame with 5 columns, samppr, SIGNAL (under 2006 conditions),
SIGNALnhi (under no human impact), SIGNAL2 (under 2006 conditions),
and SIGNAL2nhi (under no human impact)
Number of rows = that of \code{sampprPred}, (each row corresponding
  to the equivalent row in \code{sampprPred})}
}
\description{
Predict SIGNAL and SIGNAL2 scores for a set of sample-pairs
}
\details{
The sampprPreds table must contain the 10 predictor variables used in the
macroinvertebrate models: nspring, nriff, processN, AttImp_L9, AttForest_L35W1000,
CatchmentArea_km2_InclDams, meanAnnQ_mm, mnAnnAirTm_deg,
CatIgneous, and SRI_48mth_weighted. The function \code{collateBugSamppr} can
be used to compile such a table.  The model uses two boosted regression trees
that were developed from the observed SIGNAL and SIGNAL2 scores in the same 1724
samples used to calculate the 59 macroinvertebrate family models. SIGNAL scores are
calculated from the full set of collected taxa not just the 59 modelled families.
The predicted SIGNAL values are likely to differ from observed
values, which can be calculated from supplied macroinvertebrate data
using the \code{calcSIGNAL} function..
}
\examples{
mwstreamsCurrent <- bugModPred59(melbstreambiota::mwstreams)
#plotMWstreamsByVar(mwstreamsCurrent$sampprPredsLumar$lumar, nbreaks = 6,
#                   style = "fixed", fixedBreaks = c(-1,-0.25,0,0.25,0.5,0.75,1),
 #                  varName = "LUMaR", legend.cex = 0.75)
}
\seealso{
\code{\link{collateObsTable}}, \code{\link{bugModPred1}}, \code{\link{taxon.classes}}
}
