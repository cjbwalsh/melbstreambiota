#'  Find the relative contribution of families to differences in LUMaR associated with a covariate
#'
#' @param obs.table a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
#' Data for each samppr are 1s for observed presences and 0 for absences
#' @param exp.table a data.frame with the same structure as obs.table, but filled with probabilities of occurrence
#' as predicted by the 59 distribution models described by Walsh (in prep) under no human impact
#' @param covariate a vector of covariate values corresponding to samppr used as a contrast in LUMaR scores
#' @return A data frame with rows corresponding to the families found or expected in the sampprs (<= 59) and 6 columns:
#' \describe{
#'   \item{fam}{the \code{bugcode}s matched by \code{taxon.classes$fam}}
#'   \item{unexp}{0 = families whose presence contributed to differences in LUMaR by their sensitivity grade;
#'   1 = families whose presence contributed by their unexpected grade.}
#'   \item{coef}{the coefficient of a beta regression of the weighted occurrences of each families against the covariate}
#'   \item{lwr}{the 10th percentile confidence interval of coef}
#'   \item{upr}{the 90th percentile confidence interval of coef}
#'   \item{sig}{'significance': TRUE if the range between lwr and upr includes zero}
#'   }
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{predCurrNHI59}}, \code{\link{collateObsTable}}
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTable <- predCurrNHI59(sampprs)$predNHI
#' obsTable <- collateObsTable(bugData, sampprs)
#' sampprs$stream <- substr(sampprs$samppr,3,5)
#' famCovariateEffects(obsTable, expTable, sampprs$stream)
#' @export
famCovariateEffects <- function(obs.table, exp.table, covariate){
  if (!"samppr" %in% names(exp.table) & !"samppr" %in% names(obs.table)) {
    samppr <- row.names(obs.table)
    if (sum(is.na(match(names(obs.table), melbstreambiota::taxon.classes$fam))) > 0)
      stop("taxon codes do not match the 59 families in taxon.classes.
           Format expected.table using predCurrNHI59(), and obs.table using collateObsTable()", call. = FALSE)
    obs.table <- obs.table[match(names(obs.table), melbstreambiota::taxon.classes$fam)]
    exp.table <- exp.table[match(row.names(obs.table), row.names(exp.table)),match(names(obs.table), names(exp.table))]
    if (sum(row.names(obs.table) != row.names(exp.table)) + sum(names(obs.table) != names(exp.table)) > 0)
      stop("samppr names (i.e. row.names) or taxon codes in obs.table and exp.table do not match", call. = FALSE)
  }
  if ("samppr" %in% names(exp.table) & "samppr" %in% names(obs.table)) {
    samppr <- obs.table$samppr
    if (sum(is.na(match(names(obs.table)[-1], melbstreambiota::taxon.classes$fam))) > 0)
      stop("taxon codes do not match the 59 families in taxon.classes.
           Format expected.table using predCurrNHI59(), and obs.table using collateObsTable()", call. = FALSE)
    obs.table <- data.frame(samppr = as.vector(obs.table$samppr),
                            obs.table[match(names(obs.table)[-1], melbstreambiota::taxon.classes$fam) + 1])
    exp.table <- exp.table[match(exp.table$samppr, obs.table$samppr), match(names(obs.table), names(exp.table))]
    row.names(exp.table) <- exp.table$sampprs
    exp.table <- exp.table[-1]
    row.names(obs.table) <- obs.table$sampprs
    obs.table <- obs.table[-1]
    if (sum(exp.table$samppr != obs.table$samppr) + sum(names(obs.table) != names(exp.table)) > 0)
      stop("samppr names or taxon codes in obs.table and exp.table do not match")
  }
  exp.table$KG08 <- 0
  obs.wt <- obs.table*exp.table
  unexp.wt.prev <- unexp.pa.prev <- exp.prev <- exp.pa.prev <- exp.table
  for (i in 1:dim(exp.table)[2]) {
    unexp.wt.prev[,i] <- exp.prev[,i] <- exp.pa.prev[,i] <- 0
    unexp.wt.prev[round(exp.table[,i],3) < melbstreambiota::taxon.classes$pa.threshold[i],i] <-
            melbstreambiota::taxon.classes$pa.threshold[i] - exp.table[round(exp.table[,i],3) < melbstreambiota::taxon.classes$pa.threshold[i],i]
    #weight unexpected occurrences by pa.threshold minus probability of occurrence (so that more unexpected occurrences are given a higher weighting)
    exp.pa.prev[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i] <- 1
    exp.prev[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i] <-
          exp.table[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i]
  }
  obs.wt.prev <- obs.wt*exp.pa.prev
  obs.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(obs.wt.prev)))
  exp.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev)))
  unexp.wt.unexpgd.prev <- as.data.frame(t(melbstreambiota::taxon.classes$unexp.grade*t(unexp.wt.prev*obs.table)))
  exp.wt.sens.prev.sum <- apply(exp.wt.sens.prev,1,FUN = sum, na.rm = TRUE)
  obs.wt <- obs.wt.sens.prev/exp.wt.sens.prev.sum
  obs.wt <- obs.wt[,apply(obs.wt,2,FUN = sum) > 0]
  unexp.wt <- unexp.wt.unexpgd.prev/exp.wt.sens.prev.sum
  unexp.wt <- unexp.wt[,apply(unexp.wt,2,FUN = sum) > 0]
  #and exclude families present in all samples: minor differences in expected weightings
  #can result in significant linear effects particularly for categorical contrasts
  allPresent <- function(x) sum(x > 0)/length(x)
  obs.wt <- obs.wt[,apply(obs.wt, 2, FUN = allPresent) < 1]
  unexp.wt <- unexp.wt[,apply(unexp.wt, 2, FUN = allPresent) < 1]
  if(sum(obs.table$KG08) > 0){
    unexp.wt <- cbind(unexp.wt, data.frame(KG08 = obs.table$KG08*melbstreambiota::taxon.classes$unexp.grade[melbstreambiota::taxon.classes$fam == "KG08"]))
  }
  linEffects <- data.frame(fam = c(names(obs.wt),paste(names(unexp.wt),"*",sep = "")),
                           unexp = c(rep(0,dim(obs.wt)[2]),rep(1,dim(unexp.wt)[2])),
                           coef = NA, lwr = NA, upr = NA)
  for (i in 1:(dim(obs.wt)[2] + dim(unexp.wt)[2])) {
    if (i <= dim(obs.wt)[2])
      y <- 0.125 + obs.wt[,i] #(oe[,i] - min(oe))/(diff(range(oe))/0.75)
    if (i > dim(obs.wt)[2])
      y <- 0.125 + unexp.wt[,i - dim(obs.wt)[2]] #(oe[,i] - min(oe))/(diff(range(oe))/0.75)
    suppressWarnings(tryCatch({
      if (stats::var(y) == 0) {
        linEffects$coef[i] <- 0
        linEffects$lwr[i] <- 0
        linEffects$upr[i] <- 0
      }else{
      modi <- betareg::betareg(y ~ covariate)
      linEffects$coef[i] <- ifelse(i > dim(obs.wt)[2], -stats::coef(modi)[2], stats::coef(modi)[2])
      linEffects$lwr[i] <- ifelse(i > dim(obs.wt)[2], -as.data.frame(stats::confint(modi))[2,1], as.data.frame(stats::confint(modi))[2,1])
      linEffects$upr[i] <- ifelse(i > dim(obs.wt)[2], -as.data.frame(stats::confint(modi))[2,2], as.data.frame(stats::confint(modi))[2,2])
      #      if(i > dim(obs.wt)[2]) linEffects[i,c("coef","lwr","upr")] <- -1*linEffects[i,c("coef","lwr","upr")]
    }}, error = function(e){}))
  }
  linEffects$sig <- (linEffects$coef > 0 & linEffects$lwr > 0 & linEffects$upr > 0) |
    (linEffects$coef < 0 & linEffects$lwr < 0 & linEffects$upr < 0)
  linEffects <- linEffects[order(as.vector(linEffects$sig), as.vector(linEffects$unexp), linEffects$coef, decreasing = c(FALSE,FALSE,FALSE)),]
  linEffects
}

#'  Find the relative contribution of taxon-sensitivity groups to differences in LUMaR associated with a covariate
#'
#' @param lumarObject a data.frame as produced by the \code{lumar} function
#' @param covariate a vector of covariate values corresponding to samppr used as a contrast in LUMaR scores
#' @return A data frame with rows corresponding to the families found or expected in the sampprs (<= 59) and 6 columns:
#' \describe{
#'   \item{fam}{the \code{bugcode}s matched by \code{taxon.classes$fam}}
#'   \item{unexp}{0 = families whose presence contributed to differences in LUMaR by their sensitivity grade;
#'   1 = families whose presence contributed by their unexpected grade.}
#'   \item{coef}{the coefficient of a beta regression of the weighted occurrences of each families against the covariate}
#'   \item{lwr}{the 10th percentile confidence interval of coef}
#'   \item{upr}{the 90th percentile confidence interval of coef}
#'   \item{sig}{'significance': TRUE if the range between lwr and upr includes zero}
#'   }
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{predCurrNHI59}}, \code{\link{collateObsTable}}
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTables <- predCurrNHI59(sampprs)
#' obsTable <- collateObsTable(bugData, sampprs)
#' exampleLumar <- lumar(obsTable, expTables$predNHI)
#' stream <- substr(exampleLumar$samppr,3,5)
#' #What families explain differences in LUMaR between the two streams (BRS and OLN)
#' sensGpCovariateEffects(exampleLumar, stream)
#' @export
sensGpCovariateEffects <- function(lumarObject,covariate) {
  opar <- graphics::par()
  oes <- cbind(lumarObject[c("obs.wt.A", "obs.wt.B", "obs.wt.C","obs.wt.D")] /
                 (lumarObject$exp.wt.sensif.prev.nonweed + lumarObject$expdiff.prev.sensif.weedinvas),
               lumarObject[c("unexp.wt.weedy","unexp.wt.invas")])
  names(oes) <- c("oeSensA","oeSensB","oeSensC","oeSensD",
                  "ueSensWeed","ueSensInvas")
  sensGps <- c("Obs A","Obs B","Obs C","Obs D",
               "Unexp weedy","Invas")
  # oesi <- data.frame(samppr = lumarObject$samppr, covariate = covariate,
  #                   var = names(oes)[1], oe = oes[,1])
  # for (i in 2:length(names(oes))) {
  #   oesi <- rbind(oesi,
  #                 data.frame(samppr = lumarObject$samppr, covariate = covariate,
  #                            var = names(oes)[i], oe = oes[,i]))
  # }
  # graphics::par(mfrow = c(1,1))
  # graphics::par(mar = c(4,10,1,1))
  # boxplot(oesi$oe ~ oesi$covariate*oesi$var, horizontal = TRUE, las = 1)
  linEffects <- data.frame(sensGp = sensGps, coef = NA, lwr = NA, upr = NA)
  for (i in 1:length(sensGps))
  {
    if (stats::var(oes[,i]) == 0)
    {linEffects$coef[i] <- 0
    linEffects$lwr[i] <- 0
    linEffects$upr[i] <- 0
    }else{
      y <- 0.125 + (oes[,i] - min(oes)) / ((0.8 - min(oes)) / 0.75)
      modi <- betareg::betareg(y ~ covariate)
      linEffects$coef[i] <- ifelse(i > 5, -stats::coef(modi)[2], stats::coef(modi)[2])
      confinti <- as.data.frame(stats::confint(modi))
      linEffects$lwr[i] <- ifelse(i > 5, -confinti[2,1], confinti[2,1])
      linEffects$upr[i] <- ifelse(i > 5, -confinti[2,2], confinti[2,2])
    }
  }
  linEffects
}

#'  Interpretation of an observed LUMaR score at an individual site
#'
#' @param samppri a data.frame with a single row, with fields samppr, and subc
#' (a subc value drawn from mwstreams).
#' @param obsTable a data.frame with first column samppr
#' (or row.names = samppri) (the data can contain multiple rows,
#' but one row must contain the value of samppri$samppr) and 59 columnns with
#' names matching taxon.classes$fam.  ' Values = 1 for presences and 0 for absences.
#' under no human impact
#' @param currentPredTable A data.frame the same structure as obs.table, but filled with probabilities of
#' occurrence under current conditions as predicted by the 59 distribution models described by Walsh (2017).
#' @param NHIPredTable A data.frame the same structure as obs.table, but filled with probabilities of
#' occurrence under no human impact as predicted by the 59 distribution models described by Walsh (2017)
#' @param returnTable logical, if TRUE, returns 2 tables in addition to plotting results
#' @return A plot identifying variation of observed LUMaR score from predicted values under 2006
#' conditions and under no human impact, as well as the most influential families driving any differences. If returnTable
#' - TRUE a list of 2 data.frames is also returned:
#' \describe{
#'   \item{InfluencesFromRef}{...}
#'   \item{InfluencesFromPred}{...}
#'   }
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTable <- predCurrNHI59(sampprs)
#' obsTable <- collateObsTable(bugData, sampprs)
#' sampprDiagnostic(samppri = sampprs[8,],
#'                  obsTable = obsTable, currentPredTable = expTable$predCurr,
#'                  NHIPredTable = expTable$predNHI)
#' @export
sampprDiagnostic <- function(samppri, obsTable, currentPredTable, NHIPredTable,
                             returnTable = FALSE) {
  samppri$AttImp_L9 <- mwstreams$AttImp_L9[match(samppri$subc, mwstreams$subc)]
  samppri$AttForest_L35W1000 <- mwstreams$AttForest_L35W1000[match(samppri$subc, mwstreams$subc)]
  findNegInfluencesNHI <- function(obsTablei,NHIPredTablei)
  {
    weedy.index <- which(names(obsTablei) %in% c("QO02","QH67","LH01","KG05","IF61"))
    weedy.pathresholds <- c(0.626, 0.428, 0.382, 0.292, 0.416)
    invas.index <- which(names(obsTablei) == "KG08")
    obs.wt <- obsTablei*NHIPredTablei
    unexp.wt.prev <- unexp.pa.prev <- exp.prev <- exp.pa.prev <- NHIPredTablei
    unexp.wt.prev[] <- exp.prev[] <- exp.pa.prev[] <- 0
    unexp.wt.prev[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold] <-
      melbstreambiota::taxon.classes$pa.threshold[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold] - NHIPredTablei[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold]
    #weight unexpected occurrences by pa.threshold minus probability of occurrence (so that more unexpected occurrences are given a higher weighting)
    exp.pa.prev[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold] <- 1
    exp.prev[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold] <- NHIPredTablei[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold]
    obs.wt.prev <- obs.wt*exp.pa.prev
    obs.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(obs.wt.prev)))
    exp.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev)))
    # unexp.wt.unexpgd.prev <- as.data.frame(t(melbstreambiota::taxon.classes$unexp.grade*t(unexp.wt.prev*obsTablei)))
    # exp.wt.sens.prev.sum <- apply(exp.wt.sens.prev,1,FUN = sum, na.rm = TRUE)
    # unexp.wt <- unexp.wt.unexpgd.prev/exp.wt.sens.prev.sum
    # unexp.wt <- unexp.wt[apply(unexp.wt,2,FUN = sum) > 0]
    obs.expdiff <- as.data.frame(t(round(t(obs.wt),3) - melbstreambiota::taxon.classes$pa.threshold))
    obs.expdiff <- obs.expdiff*obsTablei
    obs.expdiff$KG08 <- -obsTablei$KG08
    obs.expdiffPos <- obs.expdiff#WithKG08
    obs.expdiffNeg <- obs.expdiff#WithKG08
    obs.expdiffPos[obs.expdiff < 0] <- 0  #WithKG08
    obs.expdiffNeg[obs.expdiff > 0] <- 0  #WithKG08
    expdiff.prev <- as.data.frame(t(round(t(exp.prev),3) - melbstreambiota::taxon.classes$pa.threshold))
    expdiff.prev <- expdiff.prev*exp.pa.prev #this ensures no negative numbers in this matrix
    expdiff.prev.sens <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(expdiff.prev)))
    obs.expdiffNeg.unexp <- as.data.frame(t(melbstreambiota::taxon.classes$unexp.grade*t(obs.expdiffNeg)))[weedy.index]
    if (obsTablei$KG08 == 1) {
      obs.expdiffNeg.unexp <- cbind(obs.expdiffNeg.unexp, data.frame(KG08 = -melbstreambiota::taxon.classes$unexp.grade[melbstreambiota::taxon.classes$fam == "KG08"]))
    }
    unexp.wt <- obs.expdiffNeg.unexp[,obs.expdiffNeg.unexp < 0]
    if(dim(unexp.wt)[2] > 0)
    unexp.wt <- -unexp.wt
    absTablei <- exp.pa.prev - obsTablei*exp.pa.prev
    abs.wt.nonweed <- absTablei[-weedy.index]*NHIPredTablei[-weedy.index]
    abs.wt.prev.weedy <- absTablei[weedy.index]*pmax(rep(0,5),t((NHIPredTablei[weedy.index] - weedy.pathresholds)))
    abs.wt.prev.nonweed <- abs.wt.nonweed*exp.pa.prev[-weedy.index]
    abs.wt.sens.prev.nonweed <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade[-weedy.index]*t(abs.wt.prev.nonweed)))
    abs.wt.sens.prev.weedy <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade[weedy.index]*t(abs.wt.prev.weedy)))
    abs.wt <- cbind(abs.wt.sens.prev.nonweed/sum(exp.wt.sens.prev[-weedy.index]),abs.wt.sens.prev.weedy)
    abs.wt <- abs.wt[,abs.wt > 0]
    nunexp <- ifelse(is.null(dim(unexp.wt)[2]),0,dim(unexp.wt)[2])
    nabs <- ifelse(is.null(dim(abs.wt)[2]),0,dim(abs.wt)[2])
    negInfluences <- data.frame(fam = c(names(abs.wt),names(unexp.wt)),
                                type = c(rep("Absent",nabs),rep("Unexp",nunexp)),
                                wt = c(as.vector(t(abs.wt)),as.vector(t(unexp.wt))))
    negInfluences <- negInfluences[order(negInfluences$wt, decreasing = TRUE),]
    negInfluences$cumInf <- cumsum(negInfluences$wt)/sum(negInfluences$wt)
    negInfluences
  }
  findDiffFromCurrentInfluences <- function(obsTablei,predTablei,NHIPredTablei)
  {
    weedy.index <- which(names(obsTablei) %in% c("QO02","QH67","LH01","KG05","IF61"))
    weedy.pathresholds <- c(0.626, 0.428, 0.382, 0.292, 0.416)
    obs.wt <- obsTablei*NHIPredTablei
    obs.wt.curr <- obsTablei*predTablei
    exp.prev <- exp.pa.prev <-  exp.curr.pa.prev <- NHIPredTablei  #unexp.wt.prev <-
    exp.prev[] <- exp.pa.prev[] <- exp.curr.pa.prev[] <- 0  #unexp.wt.prev[] <-
    # unexp.wt.prev[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold] <-
    #   melbstreambiota::taxon.classes$pa.threshold[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold] - NHIPredTablei[round(NHIPredTablei,3) < melbstreambiota::taxon.classes$pa.threshold]
    #weight unexpected occurrences by pa.threshold minus probability of occurrence (so that more unexpected occurrences are given a higher weighting)
    exp.pa.prev[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold] <- 1
    exp.prev[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold] <- NHIPredTablei[round(NHIPredTablei,3) >= melbstreambiota::taxon.classes$pa.threshold]
  #the next line adjusts weedy prev weightings for observed and expected weedy families
    absTablei <- exp.pa.prev - obsTablei*exp.pa.prev
    exp.prev[weedy.index] <- pmax(rep(0,5),t((NHIPredTablei[weedy.index] - weedy.pathresholds)))
    exp.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev)))
    exp.wt.sens.prev.sum <- apply(exp.wt.sens.prev,1,FUN = sum, na.rm = TRUE)
    exp.curr.pa.prev[round(predTablei,3) >= melbstreambiota::taxon.classes$pa.threshold] <- 1
    exp.curr.prev  <- exp.curr.pa.prev*NHIPredTablei*exp.pa.prev
    exp.curr.wt.sens.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.curr.prev)))

    exp.curr.unexp <- as.data.frame(t(pmin(rep(0,5),round(t(NHIPredTablei[weedy.index]),3)
                                                                   - weedy.pathresholds)))*
                                          exp.curr.pa.prev[weedy.index]
    names(exp.curr.unexp) <- melbstreambiota::taxon.classes$fam[weedy.index]
    exp.curr.unexp.wt <- exp.curr.unexp*melbstreambiota::taxon.classes$unexp.grade[weedy.index]
    obs.exp.curr.unexp.wt <- exp.curr.unexp.wt*obsTablei[weedy.index]
    obs.exp.curr.unexp.wt <- obs.exp.curr.unexp.wt[,obs.exp.curr.unexp.wt < 0]

    #1. Present, unexpected under NHI, unexpected under current: obs.unexp.curr.unexp.wt
    #These make the observed lumar lower than the predicted current lumar, including KG08
    unexp.curr.unexp <- as.data.frame(t(pmin(rep(0,5),round(t(NHIPredTablei[weedy.index]),3)
                                           - weedy.pathresholds)))*
                                           !exp.curr.pa.prev[weedy.index]
    names(unexp.curr.unexp) <- melbstreambiota::taxon.classes$fam[weedy.index]
    unexp.curr.unexp.wt <- unexp.curr.unexp*melbstreambiota::taxon.classes$unexp.grade[weedy.index]
    obs.unexp.curr.unexp.wt <- unexp.curr.unexp*obsTablei[weedy.index]
    obs.unexp.curr.unexp.wt <- obs.unexp.curr.unexp.wt[,obs.unexp.curr.unexp.wt < 0]
    if(obsTablei$KG08 == 1 & exp.curr.pa.prev$KG08 == 0)
      obs.unexp.curr.unexp.wt <- cbind(obs.unexp.curr.unexp.wt,
                                data.frame(KG08 = -melbstreambiota::taxon.classes$unexp.grade[melbstreambiota::taxon.classes$fam == "KG08"]))

    #2. Absent, unexpected under NHI but expected under current: notobs.exp.curr.unexp.wt
        #These make the observed lumar lower than the predicted current lumar,
         #except KG08, which has the opposite effect
    notobs.exp.curr.unexp.wt <- exp.curr.unexp.wt*!obsTablei[weedy.index]
    notobs.exp.curr.unexp.wt <- as.data.frame(notobs.exp.curr.unexp.wt[,notobs.exp.curr.unexp.wt < 0])
    names(notobs.exp.curr.unexp.wt) <- names(exp.curr.unexp)[!obsTablei[weedy.index] & exp.curr.unexp.wt < 0]
    if(obsTablei$KG08 == 0 & exp.curr.pa.prev$KG08 == 1)
    notobs.exp.curr.unexp.wt <- cbind(notobs.exp.curr.unexp.wt,
                                      data.frame(KG08 = melbstreambiota::taxon.classes$unexp.grade[melbstreambiota::taxon.classes$fam == "KG08"]))

    ##3. Present, expected under NHI, but unexpected under current: obs.expNHI.unexpCurr.sensif
    ##These make the observed lumar higher than the predicted current lumar
    obs.expNHI.unexpCurr <- !exp.curr.pa.prev & exp.pa.prev & obsTablei
    obs.expNHI.unexpCurr.sensif <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev*obs.expNHI.unexpCurr)))
    nametemp <- names(obs.expNHI.unexpCurr.sensif)[obs.expNHI.unexpCurr.sensif > 0]
    obs.expNHI.unexpCurr.sensif <- as.data.frame(obs.expNHI.unexpCurr.sensif[,obs.expNHI.unexpCurr.sensif > 0])
    names(obs.expNHI.unexpCurr.sensif) <- nametemp

    ##4. Absent but expected under current and NHI: notobs.exp.currNHI.wt.sens
    #These make the observed lumar lower than the predicted current lumar
    exp.currNHI.not.obs <- exp.pa.prev & exp.curr.pa.prev & !obsTablei
    notobs.exp.currNHI.wt.sens <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev*exp.currNHI.not.obs)))
    nametemp <- names(notobs.exp.currNHI.wt.sens)[notobs.exp.currNHI.wt.sens > 0]
    notobs.exp.currNHI.wt.sens <- as.data.frame(notobs.exp.currNHI.wt.sens[,notobs.exp.currNHI.wt.sens > 0])
    names(notobs.exp.currNHI.wt.sens) <- nametemp
    if(dim(notobs.exp.currNHI.wt.sens)[2] > 0){
      notobs.exp.currNHI.wt.sens <- -notobs.exp.currNHI.wt.sens
}
    dim1 <- ifelse(is.null(dim(obs.unexp.curr.unexp.wt)[2]),0,dim(obs.unexp.curr.unexp.wt)[2])
    dim2 <- ifelse(is.null(dim(notobs.exp.curr.unexp.wt)[2]),0,dim(notobs.exp.curr.unexp.wt)[2])
    dim3 <- ifelse(is.null(dim(obs.expNHI.unexpCurr.sensif)[2]),0,dim(obs.expNHI.unexpCurr.sensif)[2])
    dim4 <- ifelse(is.null(dim(notobs.exp.currNHI.wt.sens)[2]),0,dim(notobs.exp.currNHI.wt.sens)[2])
    Influences <- data.frame(fam = c(names(obs.unexp.curr.unexp.wt),
                                     names(notobs.exp.curr.unexp.wt),
                                     names(obs.expNHI.unexpCurr.sensif),
                                     names(notobs.exp.currNHI.wt.sens)),
                             #1. Present, unexpected under NHI, unexpected under current: obs.unexp.curr.unexp.wt
                             #2. Absent, unexpected under NHI but expected under current: notobs.exp.curr.unexp.wt                             type = c(rep("Present, unexpected under NHI, unexpected under current",dim1),
                             #3. Present, expected under NHI, but unexpected under current: obs.expNHI.unexpCurr.sensif
                             #4. Absent but expected under current and NHI: notobs.exp.currNHI.wt.sens
                             type = c(rep("Present, unexpected under NHI, unexpected under current",dim1),
                                      rep("Absent, unexpected under NHI but expected under current",dim2),
                                      rep("Present, expected under NHI, but unexpected under current", dim3),
                                      rep("Absent but expected under current and NHI", dim4)),
                             wt = c(as.vector(t(obs.unexp.curr.unexp.wt)),
                                    as.vector(t(notobs.exp.curr.unexp.wt)),
                                    as.vector(t(obs.expNHI.unexpCurr.sensif))/exp.wt.sens.prev.sum,
                                    as.vector(t(notobs.exp.currNHI.wt.sens))/exp.wt.sens.prev.sum))
 #   Influences$wt[Influences$influence == "negative"] <- -1*Influences$wt[Influences$influence == "negative"]
    Influences <- Influences[order(Influences$wt, decreasing = TRUE),]
    Influences
  }
  mwstreams <- melbstreambiota::mwstreams
  meds <- CIs$lumarCIs$median
  if (names(obsTable)[1] == "samppr") {
    row.names(obsTable) <- as.vector(obsTable[,1])
    obsTable <- obsTable[-1]
    row.names(currentPredTable) <- as.vector(currentPredTable[,1])
    currentPredTable <- currentPredTable[-1]
    row.names(NHIPredTable) <- as.vector(NHIPredTable[,1])
    NHIPredTable <- NHIPredTable[-1]
  }
  ordTaxon.classes <- melbstreambiota::taxon.classes[match(names(obsTable), melbstreambiota::taxon.classes$fam),]
  obsTablei <- obsTable[row.names(obsTable) == samppri$samppr,]
  NHIPredTablei <- NHIPredTable[row.names(NHIPredTable) == samppri$samppr,
                                match(names(obsTable), names(NHIPredTable))]
  NHIPredTablei$KG08 <- 0
  currentPredPATablei <- currentPredTablei <-
    currentPredTable[row.names(currentPredTable) == samppri$samppr,
                     match(names(currentPredTable), names(NHIPredTable))]
  currentPredPATablei[round(currentPredTablei,3) < ordTaxon.classes$pa.threshold] <- 0
  currentPredPATablei[round(currentPredTablei,3) >= ordTaxon.classes$pa.threshold] <- 1
  lumarPred <- lumar(currentPredPATablei, NHIPredTablei)
  lumarPredLwr <- CIs$lumarCIs$lwr10[which.min(abs(meds - lumarPred$lumar))[1]]
  lumarPredUpr <- CIs$lumarCIs$upr10[which.min(abs(meds - lumarPred$lumar))[1]]
  lumarObs <- lumar(obsTablei, NHIPredTablei)
  # lumarObs$lumar < CIs$lumarCIs$lwr10[round(lumarObs)]
  samppri$ai <- mwstreams$AttImp_L9[match(samppri$subc, mwstreams$site)]
  samppri$af <- mwstreams$AttForest_L35W1000[match(samppri$subc, mwstreams$site)]
  #1. missing and unexpected compared to NHI (assuming if >=0.8, it is as good as NHI...)
  if (lumarObs$lumar < 1)
  {
    negInfluences <- findNegInfluencesNHI(obsTablei,NHIPredTablei)
    # Lt0.8 <- 0.8 - lumarObs$lumar
    # negInf0.8 <- negInfluences[c(0, which(negInfluences$cumInf < Lt0.8)) + 1,]
    negInfluences$inf <- negInfluences$wt
#    negInfluences$inf[-1] <-  negInfluences$cumInf[-1] - negInfluences$cumInf[-length(negInfluences$cumInf)]
    negInfluences$inf <- -1*negInfluences$inf
    negInfluences$family <- ordTaxon.classes$family[match(negInfluences$fam, ordTaxon.classes$fam)]
    negInfluences$sens.gp <- ordTaxon.classes$sens.gp[match(negInfluences$fam, ordTaxon.classes$fam)]
    negInfluences$family <- paste(as.vector(negInfluences$family)," (",as.vector(negInfluences$sens.gp), ")",c("","*")[match(negInfluences$type,c("Absent","Unexp"))],sep = "")
  }
  if (lumarObs$lumar < lumarPredLwr | lumarObs$lumar > lumarPredUpr) {
    Influences <- findDiffFromCurrentInfluences(obsTablei,currentPredTablei,NHIPredTablei)
    Influences$family <- ordTaxon.classes$family[match(Influences$fam, ordTaxon.classes$fam)]
    Influences$sens.gp <- ordTaxon.classes$sens.gp[match(Influences$fam, ordTaxon.classes$fam)]
    Influences$family <- paste(as.vector(Influences$family)," (",as.vector(Influences$sens.gp), ")",c("","*")[match(grepl("Absent",Influences$type),c(1,0))],sep = "")
  }
  #
  graphics::layout(matrix(c(1,2,3,3,4,4),2,3,byrow = FALSE),heights = c(6,4),widths = c(6,10,10))
  graphics::par(mar = c(1,4,1,1))
  graphics::plot(c(1,1), c(lumarPred$lumar, lumarObs$lumar), ylim = c(-1,1),
       axes = FALSE, ylab = "LUMaR",xlab = "",type = 'n')
  graphics::points(c(1,1), c(lumarPred$lumar, lumarObs$lumar),
         pch = c(21,22), bg = c("black","white"), cex = c(1.5,1))
  graphics::lines(c(1,1),c(lumarPredLwr,lumarPredUpr))
  graphics::axis(2,at = seq(-1,1,0.2), labels = round(seq(-1,1,0.2),1),las = 1)
  graphics::plot.new()
  graphics::par(mar = c(0,0,0,0))
  graphics::legend("center", pch = c(21,22,NA,NA), pt.bg = c("black","white"), pt.cex = c(1.5,1),
                    legend = c("Predicted with 95% CIs", "Observed",
                    paste("AI = ", round(signif(samppri$AttImp_L9*100,2),1),"%",sep = ""),
                    paste("AF = ", round(signif(samppri$AttForest_L35W1000*100,2),1),"%",sep = "")))
  graphics::par(mar = c(4,10,3,1))
  if(lumarObs$lumar >= 0.8 & !(lumarObs$lumar < lumarPredLwr | lumarObs$lumar > lumarPredUpr)){
    graphics::plot.new()
    graphics::title(main = "This site is predicted to be in reference condition and", adj = 1,
                    font.main = 1, cex.main = 0.9)
    graphics::title(main = "observed LUMaR is within the predicted range", adj = 1,
                    font.main = 1, cex.main = 0.9, line = 0)
  }
  if (lumarObs$lumar < 0.8)
  {
    graphics::plot(negInfluences$inf,dim(negInfluences)[1]:1, xlim = c(min(negInfluences$inf),0), axes = FALSE,
         ylab = "",xlab = "Reduction in LUMaR")
    graphics::axis(2, at = dim(negInfluences)[1]:0,labels = c(negInfluences$family,""), las = 1)
    graphics::axis(1, at = seq(-2,0,0.01))
    graphics::abline(v = 0, lty = 3)
    graphics::title(main = "Contributions of families to LUMaR being < 1 (reference)", adj = 1,
          font.main = 1, cex.main = 0.9)
#    graphics::title(main = "Observed LUMaR is close to reference condition (>0.8)", adj = 1,
#          font.main = 3, cex.main = 0.9, line = 0.25)
    if (lumarObs$lumar < lumarPredLwr | lumarObs$lumar > lumarPredUpr) {
      graphics::plot(Influences$wt,1:dim(Influences)[1], axes = FALSE,
           ylab = "",xlab = "Contribution to difference in LUMaR")
      graphics::axis(2, at = 0:dim(Influences)[1],labels = c("",Influences$family), las = 1)
      graphics::axis(1, at = seq(-2,2,0.01))
      graphics::abline(v = 0, lty = 3)
      graphics::title(main = paste("Contributions of families to LUMaR being ",
                         ifelse(lumarObs$lumar < lumarPredLwr, "<",">"),
                         "predicted LUMaR", sep = ""), adj = 1,
            font.main = 1, cex.main = 0.9)
    }else{
      graphics::plot.new()
      graphics::title(main = "Observed LUMaR is within predicted range for this site", adj = 1,
            font.main = 1, cex.main = 0.9)
      Influences <- NULL
    }
    graphics::title(main = paste("assuming AI = ",
                       round(signif(samppri$AttImp_L9*100,2),1),
                       "% and AF = ", round(signif(samppri$AttForest_L35W1000*100,2),1), "%", sep = ""), adj = 1,
          font.main = 3, cex.main = 0.9, line = 0.25)
  }
  if (returnTable == TRUE)
    list(InfluencesFromRef = negInfluences, influencesFromPred = Influences)
}

#'  Which families drive differences in LUMaR between groups of sample-pairs?
#'
#' Diagnostic plots to identify the most influential families influencing differences between groups of sample pairs,
#' a trend among sample-pairs explained by a covariate
#'
#' @param lumarObject a data.frame produced by the function \code{lumar}
#' @param covariate a vector length = number of rows in lumarObject (that explains some of the variation in LUMaR scores)
#' @param covariateName a character vector for labelling the output plot
#' @param sensLinEffects ...
#' @param famLinEffects ...
#' @return A plot identifying variation of observed LUMaR score from predicted values under 2006
#' conditions and under no human impact, as well as the most influential families driving any differences. If returnTable
#' - TRUE a list of 2 data.frames is also returned:
#' \describe{
#'   \item{InfluencesFromRef}{...}
#'   \item{InfluencesFromPred}{...}
#'   }
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                 package = "melbstreambiota", mustWork = TRUE),
#'                                     sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTables <- predCurrNHI59(sampprs)
#' obsTable <- collateObsTable(bugData, sampprs)
#' exampleLumar <- lumar(obsTable, expTables$predNHI)
#' stream <- substr(exampleLumar$samppr, 1, 3)
#' # What families explain differences in LUMaR between the two streams (BRS and OLN)
#' famStreamEffects <- famCovariateEffects(obsTable, expTables$predNHI, stream)
#' sensStreamEffects <- sensGpCovariateEffects(exampleLumar, stream)
#' plotSensFamDiagnostics(exampleLumar, stream, covariateName = "stream",
#'                        sensLinEffects = sensStreamEffects,
#'                        famLinEffects = famStreamEffects)
#' @export
plotSensFamDiagnostics <- function(lumarObject, covariate, covariateName = "Covariate",
                                   sensLinEffects, famLinEffects) {
  graphics::layout(matrix(c(1,2,3,3),2,2,byrow = FALSE),heights = c(6,8),widths = c(6,6))
  graphics::par(mar = c(4,4,1,1))
  if (!is.numeric(covariate))
    cv <- match(covariate,unique(covariate))
  if (is.numeric(covariate))
    cv <- covariate
  graphics::plot(jitter(cv, amount = 0), jitter(lumarObject$lumar, amount = 0), ylab = "LUMaR score",
       xlab = "", las = 1, axes = FALSE,ylim = c(min(lumarObject$lumar),1))
  graphics::axis(2,seq(-0.4,1,0.2), las = 1)
  if (!is.numeric(covariate))
    graphics::axis(1, at = unique(cv), labels = unique(covariate))
  if (is.numeric(covariate))
    graphics::axis(1)
  graphics::title(xlab = covariateName, line = 2)
  graphics::title(main = "A. Variation in LUMaR score", adj = 0, font.main = 1, cex.main = 0.9)

  graphics::par(mar = c(4,6,1,1))
  sle <- sensLinEffects[6:1,]
  graphics::plot(sle$coef, 1:6,xlim = range(sle[,-1]),axes = FALSE,
       xlab = "Effect of covariate", ylab = "")
  graphics::axis(1, at = seq(-6,2,0.5))
  graphics::axis(2,at = 0:6,labels = c("",as.vector(sle$sensGp)), las = 1)
  for (i in 1:6)
    graphics::lines(sle[i,3:4],c(i,i))
  graphics::abline(v = 0, lty = 3)
  graphics::title(main = "B. Effect of each Sensitivity group", adj = 0, font.main = 1, cex.main = 0.9)

  le <- famLinEffects[famLinEffects$sig,]
  le <- le[order(as.vector(le$sig), le$coef, decreasing = c(FALSE,FALSE)),]
  le$family <- melbstreambiota::taxon.classes$family[match(gsub("\\*","",le$fam), melbstreambiota::taxon.classes$fam)]
  le$sens.gp <- melbstreambiota::taxon.classes$sens.gp[match(gsub("\\*","",le$fam), melbstreambiota::taxon.classes$fam)]
  le$unexp[le$unexp == 0] <- ""
  le$unexp[le$unexp == 1] <- "*"
  graphics::par(mar = c(4,10,1,1))
  graphics::plot(le$coef, 1:dim(le)[1], xlim = range(c(le$lwr,le$upr)), axes = FALSE,
       xlab = "Effect of covariate",ylab = "")
  for (i in 1:dim(le)[1])
    graphics::lines(c(le$lwr[i],le$upr[i]),c(i,i))
  graphics::abline(v = 0, lty = 3)
  graphics::axis(1,at = seq(-4,2,0.5))
  graphics::axis(2, at = 0:dim(le)[1],labels = c("",paste(le$family, " (",le$sens.gp,")",
                                                le$unexp,sep = "")), las = 1)
  graphics::title(main = "C. Effect of influential families", adj = 0, font.main = 1, cex.main = 0.9)
}

