#' Predict occurrence of a macroinvertebrate family in a set of sample-pairs
#'
#' @param bugcode a 4-character code representing a macroivertebrate family:
#' one of the 59 \code{taxon.classes$fam}s
#' @param sampprPreds a data.frame of sample-pair codes (samppr) each with
#' associated environmental predictors.
#' @return A list of two vectors with length = number of rows in \code{sampprPred},
#' with each row corresponding to the equivalent row in \code{sampprPred}.
#' \describe{
#'   \item{pred1}{predicted probabilities of occurrence under conditions specified in \code{sampprPred}}
#'   \item{pred1NHI}{predicted probabilities of occurrence under no human impact
#'   (AttImp_L9 set to zero, AttForest_L35W1000 set to 1)}
#'   }
#' @details The sampprPreds table must contain the 10 predictor variables used in the
#' models: nspring,nriff,processN,AttImp_L9, AttForest_L35W1000,
#' CatchmentArea_km2_InclDams, meanAnnQ_mm,mnAnnAirTm_deg,
#' CatIgneous, and SRI_48mth_weighted. The function \code{collateBugSamppr} can
#' be used to compile such a table
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{taxon.classes}}
#' @examples
#' qt25MWstreamsCurrent <- bugModPred1("QT25",melbstreambiota::mwstreams)
#' plotMWstreamsByVar(qt25MWstreamsCurrent$pred1,
#'    style = "fixed", fixedBreaks = seq(0,1,0.2),
#'    varName =
#'     paste(melbstreambiota::taxon.classes$family[melbstreambiota::taxon.classes$fam == "QT25"],
#'           "prob of occurrence"), legend.cex = 0.75)
#' @export
bugModPred1 <- function(bugcode,
                        sampprPreds)
{
  brt.model <- bestModelsBugfams[bugcode][[1]]
  pred1 <- gbm::predict.gbm(brt.model, sampprPreds, n.trees = brt.model$gbm.call$best.trees, type = "response")
  pred1pa <- vector("numeric", length = length(pred1))
  pred1pa[round(pred1,3) >= melbstreambiota::taxon.classes$pa.threshold[melbstreambiota::taxon.classes$fam == bugcode]] <- 1
  sampprPreds1 <- sampprPreds
  sampprPreds1$AttImp_L9 <- 0; sampprPreds1$AttForest_L35W1000 = 1
  pred1NHI <- gbm::predict.gbm(brt.model, sampprPreds1, n.trees = brt.model$gbm.call$best.trees, type = "response")
  list(pred1 = pred1, pred1NHI = pred1NHI)
}

#' Predict occurrence of a fish species or platypus (total or female) in a set of subcatchments
#'
#' @param code an 8-character code as listed in \code{vertSpp}
#' @param vertSamps a data.frame of subcatchments (from \code{mwstreams}) each with
#' associated environmental predictors.
#' @return A vector with length = number of rows in \code{vertSamps},
#' with each row corresponding to the equivalent row in \code{vertSamps}.
#' \describe{predicted probabilities of occurrence under conditions specified in \code{sampprPred}}
#' @details The vertSamps table must contain the 9 predictor variables used in the
#' models. The function \code{collateSampleFP} can
#' be used to compile such a table. (Note platypus and fish species have different predictors)
#' @seealso \code{\link{collateSampleFP}}, \code{\link{vertSpp}}
#' @export
vertPred <- function(code, vertSamps){
  brt.model <- bestModelsVerts[code][[1]]
  sampPred <- gbm::predict.gbm(brt.model, vertSamps, n.trees = brt.model$gbm.call$best.trees, type = "response")
  sampPred
}

#' Compile predictions of occurrence for all 59 macroinvertebrate families in a set of sample-pairs
#'
#' @param sampprPreds a data.frame of sample-pair codes (samppr) each with
#' associated environmental predictors. If there is no field called samppr, then the field subc if used as samppr.
#' @return A list of two dataframes with number of rows = that of \code{sampprPred},
#' (each row corresponding to the equivalent row in \code{sampprPred}) and 59
#' columns, corresponding to the 59 families listed in \code{taxon.classes}
#' \describe{
#'   \item{predCurr}{predicted probabilities of occurrence under conditions specified in \code{sampprPred}}
#'   \item{predNHI}{predicted probabilities of occurrence under no human impact
#'   (AttImp_L9 set to zero, AttForest_L35W1000 set to 1)}
#'   }
#' @details The sampprPreds table must contain the 10 predictor variables used in the
#' models: nspring,nriff,processN,AttImp_L9, AttForest_L35W1000,
#' CatchmentArea_km2_InclDams, meanAnnQ_mm,mnAnnAirTm_deg,
#' CatIgneous, and SRI_48mth_weighted. The function \code{collateBugSamppr} can
#' be used to compile such a table.  Use \code{bugModPred1} to make equivalent predictions for a single taxon.
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{bugModPred1}}, \code{\link{taxon.classes}}
#' @export
predCurrNHI59 <- function(sampprPreds)
{
  if (!"samppr" %in% names(sampprPreds))
    sampprPreds$samppr <-  sampprPreds$subc
  if (!"samppr" %in% names(sampprPreds) & !"subc" %in% names(sampprPreds))
    stop("sampprPreds must have either a field 'samppr' or a field 'subc'", call. = FALSE)
    pred59 <- data.frame(matrix(0, ncol = dim(melbstreambiota::taxon.classes)[1] + 1,
                              nrow = dim(sampprPreds)[1]))
  names(pred59) <- c("samppr", melbstreambiota::taxon.classes$fam)
  pred59$samppr <- as.vector(sampprPreds$samppr)
  pred59NHI <- pred59
  for (i in 1:length(bestModelsBugfams))
  {
    predsi <- bugModPred1(names(bestModelsBugfams)[i], sampprPreds)
    pred59[,i + 1] <- predsi$pred1
    pred59NHI[,i + 1] <- predsi$pred1NHI
  }
  list(predCurr = pred59, predNHI = pred59NHI)
}

#' Compile predictions of occurrence for all 59 macroinvertebrate families and resulting indices in a set of sample-pairs
#'
#' @param sampprPreds a data.frame of sample-pair codes (samppr) each with
#' associated environmental predictors. If there is no field called samppr, then the field subc if used.
#' @return A list containing:
#' two dataframes with number of rows = that of \code{sampprPreds},
#' (each row corresponding to the equivalent row in \code{sampprPred}) and 59
#' columns, corresponding to the 59 families listed in \code{taxon.classes}
#' \describe{
#'   \item{sampprPredsLumar}{ data frame with 4 columns, samppr, LUMaR, SIGNAL2, and number
#'   of sensitive families (nSensFams) resulting from the predicted probabilities of
#'   occurrence.  Number of rows = that of \code{sampprPred}, (each row corresponding
#'   to the equivalent row in \code{sampprPred})}
#'   \item{pred59}{data frame containing predicted probabilities of occurrence
#'   under conditions specified in \code{sampprPred}, with number of
#'   rows = that of \code{sampprPred}, (each row corresponding to the
#'   equivalent row in \code{sampprPred}) and 59 columns, corresponding
#'   to the 59 families listed in \code{taxon.classes}}
#'   }
#' @details The sampprPreds table must contain the 10 predictor variables used in the
#' models: nspring, nriff, processN, AttImp_L9, AttForest_L35W1000,
#' CatchmentArea_km2_InclDams, meanAnnQ_mm, mnAnnAirTm_deg,
#' CatIgneous, and SRI_48mth_weighted. The function \code{collateBugSamppr} can
#' be used to compile such a table.  Use \code{bugModPred1} to make equivalent predictions for a single family.
#' Note that the LUMaR and number of sensitive families produced by this function are PREDICTED values
#' given the environmental variables supplied in sampprPreds.  These values are likely to differ from observed
#' values that can be calculated using the lumar function from your own appropriately processed
#' macroinvertebrate data (see the melbstreambiota vignette for instructions).
#' to the obs.table argument in the \code{lumar} function.
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{bugModPred1}}, \code{\link{taxon.classes}}
#' @examples
#' mwstreamsCurrent <- bugModPred59(melbstreambiota::mwstreams)
#' plotMWstreamsByVar(mwstreamsCurrent$sampprPredsLumar$lumar, nbreaks = 6,
#'                    style = "fixed", fixedBreaks = c(-1,-0.25,0,0.25,0.5,0.75,1),
#'                    varName = "LUMaR", legend.cex = 0.75)
#' @export
bugModPred59 <- function(sampprPreds)
{
  if (!"samppr" %in% names(sampprPreds) & !"subc" %in% names(sampprPreds))
    stop("sampprPreds must have either a field 'samppr' or a field 'subc'", call. = FALSE)
  if (!"samppr" %in% names(sampprPreds))
    sampprPreds$samppr <- sampprPreds$subc
  pred59 <- data.frame(matrix(0, ncol = dim(melbstreambiota::taxon.classes)[1] + 1,
                              nrow = dim(sampprPreds)[1]))
  names(pred59) <- c("samppr", melbstreambiota::taxon.classes$fam)
  pred59$samppr <- sampprPreds$samppr
  pred59pa <- pred59NHI <- pred59
  for (i in 1:length(bestModelsBugfams))
  {
    predsi <- bugModPred1(names(bestModelsBugfams)[i], sampprPreds)
    pred59[,i + 1] <- predsi$pred1
    pred59pa[round(pred59[,i + 1], 3) >= melbstreambiota::taxon.classes$pa.threshold[i], i + 1] <- 1
    pred59NHI[,i + 1] <- predsi$pred1NHI
  }
  sampprPredsLumar <- lumar(pred59pa[,-1], pred59NHI[,-1])
  sampprPredsLumar$nSensFams <- apply(pred59pa[-1][melbstreambiota::taxon.classes$sens.gp %in% LETTERS[1:4]],1, FUN = sum)
  sigtemp <- t(pred59pa[-1])*melbstreambiota::taxon.classes$SIGNAL2
  sigtemp[sigtemp == 0] <- NA
  sampprPredsLumar$SIGNAL2 <- apply(sigtemp, 2, mean, na.rm = TRUE)
  sampprPredsLumar$samppr <- sampprPreds$samppr
  list(sampprPredsLumar = sampprPredsLumar, pred59 = pred59)
}

#' Predict SIGNAL and SIGNAL2 scores for a set of sample-pairs
#'
#' @param sampprPreds a data.frame of sample-pair codes (samppr) each with
#' associated environmental predictors. If there is no field called samppr,
#' then the field subc if used.
#' @return {A data frame with 5 columns, samppr, SIGNAL (under 2006 conditions),
#' SIGNALnhi (under no human impact), SIGNAL2 (under 2006 conditions),
#' and SIGNAL2nhi (under no human impact)
#' Number of rows = that of \code{sampprPred}, (each row corresponding
#'   to the equivalent row in \code{sampprPred})}
#' @details The sampprPreds table must contain the 10 predictor variables used in the
#' macroinvertebrate models: nspring, nriff, processN, AttImp_L9, AttForest_L35W1000,
#' CatchmentArea_km2_InclDams, meanAnnQ_mm, mnAnnAirTm_deg,
#' CatIgneous, and SRI_48mth_weighted. The function \code{collateBugSamppr} can
#' be used to compile such a table.  The model uses two boosted regression trees
#' that were developed from the observed SIGNAL and SIGNAL2 scores in the same 1724
#' samples used to calculate the 59 macroinvertebrate family models. SIGNAL scores are
#' calculated from the full set of collected taxa not just the 59 modelled families.
#' The predicted SIGNAL values are likely to differ from observed
#' values, which can be calculated from supplied macroinvertebrate data
#' using the \code{calcSIGNAL} function..
#' @seealso \code{\link{collateObsTable}}, \code{\link{bugModPred1}}, \code{\link{taxon.classes}}
#' @examples
#' mwstreamsCurrent <- bugModPred59(melbstreambiota::mwstreams)
#' plotMWstreamsByVar(mwstreamsCurrent$sampprPredsLumar$lumar, nbreaks = 6,
#'                    style = "fixed", fixedBreaks = c(-1,-0.25,0,0.25,0.5,0.75,1),
#'                    varName = "LUMaR", legend.cex = 0.75)
#' @export
bugModPredSIGNAL <- function(sampprPreds)
{
  if (!"samppr" %in% names(sampprPreds) & !"subc" %in% names(sampprPreds))
    stop("sampprPreds must have either a field 'samppr' or a field 'subc'", call. = FALSE)
  if (!"samppr" %in% names(sampprPreds))
    sampprPreds$samppr <- sampprPreds$subc
  sampprPreds1 <- sampprPreds
  sampprPreds1$AttImp_L9 <- 0; sampprPreds1$AttForest_L35W1000 = 1
  brt.model <- bestModelsSIGNAL$signalMod
  pred1 <- gbm::predict.gbm(brt.model, sampprPreds, n.trees = brt.model$gbm.call$best.trees, type = "response")
  pred1NHI <- gbm::predict.gbm(brt.model, sampprPreds1, n.trees = brt.model$gbm.call$best.trees, type = "response")
  brt.model <- bestModelsSIGNAL$signal2Mod
  pred2 <- gbm::predict.gbm(brt.model, sampprPreds, n.trees = brt.model$gbm.call$best.trees, type = "response")
  pred2NHI <- gbm::predict.gbm(brt.model, sampprPreds1, n.trees = brt.model$gbm.call$best.trees, type = "response")
  preds <- data.frame(samppr = sampprPreds$samppr,
                      SIGNAL = pred1, SIGNALnhi = pred1NHI,
                      SIGNAL2 = pred2, SIGNAL2nhi = pred2NHI)
  preds
}
