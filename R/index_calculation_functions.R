#' Bray-Curtis similarity of two concatenated vectors
#'
#'  A function to calculate Bray-Curtis dissimilarity index of two vectors that
#'  can be used in \code{apply} to return a vector of BCs for each row (used to calculate \code{simOE} in \code{oestats})
#
#'
#' @param x a vector composed of the two vectors concatenated together.
#' @return Bray-Curtis similarity of the two concatenated vectors
#' @seealso \code{\link[vegan]{vegdist}}
#' @examples
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 3))
#' #Bray-Curtis similarity of first 2 sampprs
#' ab <- unlist(c(bugData[1,-1],bugData[2,-1]))
#' bray.2halves(ab)
#' @export
bray.2halves <- function(x)

{
  a <- x[1:(length(x)/2)]
  b <- x[(length(x)/2 + 1):length(x)]
  1 - vegan::vegdist(rbind(a,b), na.rm = TRUE)
}

#' Calculate LUMaR index and associated statistics from observed and expected tables
#'
#'  Calculate LUMaR index and associated statistics from observed and expected tables
#'  formatted to include the 59 families in the same order as taxon.classes$fam
#
#'
#' @param obs.table a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
#' Data for each samppr are 1s for observed presences and 0 for absences
#' @param exp.table a data.frame with the same structure as obs.table, but filled with probabilities of occurrence
#' as predicted by the 59 distribution models described by Walsh (2023, https://osf.io/392kv) under no human impact
#' @return A data frame with the same number of rows as the input tables and 14 columns: the LUMaR index and its components
#'#' \describe{
#'   \item{samppr}{\code{samppr} as in input tables}
#'   \item{lumar}{LUMaR index}
#'   \item{nSensFams}{Number of sensitive families (as defined by Walsh 2017) in each sample}
#'   \item{obs.wt.sens}{}
#'   \item{obs.wt.unexp}{}
#'   \item{obs.wt.A}{}
#'   \item{obs.wt.B}{}
#'   \item{obs.wt.C}{}
#'   \item{obs.wt.D}{}
#'   \item{obs.wt.weedy}{}
#'   \item{unexp.wt.B}{}
#'   \item{unexp.wt.D}{}
#'   \item{unexp.wt.weedy}{}
#'   \item{unexp.wt.invas}{}
#'   \item{exp.wt}{}
#'   }
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{predCurrNHI59}}, \code{\link{collateObsTable}}
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package = "melbstreambiota", mustWork = TRUE),
#'                                         sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package = "melbstreambiota", mustWork = TRUE),
#'                                         sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTable <- predCurrNHI59(sampprs)$predNHI
#' obsTable <- collateObsTable(bugData, sampprs)
#' lumar(obsTable, expTable)
#' @export
lumar <- function(obs.table, exp.table)
{
  if (!"samppr" %in% names(exp.table) & !"samppr" %in% names(obs.table)) {
    samppr <- row.names(obs.table)
    if (sum(is.na(match(names(obs.table), melbstreambiota::taxon.classes$fam))) > 0)
      stop("taxon codes do not match the 59 families in taxon.classes.
           Format expected.table using predCurrNHI59(), and obs.table using collateObsTable()", call. = FALSE)
  obs.table <- obs.table[match(names(obs.table), melbstreambiota::taxon.classes$fam)]
  exp.table <- exp.table[match(row.names(obs.table), row.names(exp.table)),match(names(obs.table), names(exp.table))]
  if (sum(row.names(obs.table) != row.names(exp.table))+ sum(names(obs.table) != names(exp.table)) > 0)
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
  weedy.index <- which(names(obs.table) %in% melbstreambiota::taxon.classes$fam[melbstreambiota::taxon.classes$sens.gp == "weedy"] & names(obs.table) != "KG08")
  invas.index <- which(names(obs.table) == "KG08")
  #set expected occurrence of exotic Physidae to zero
  exp.tableWithKG08 <- exp.table
  exp.table$KG08 <- 0
  obs.wt <- obs.table*exp.table

  unexp.wt.prev <- unexp.pa.prev <- exp.prev <- exp.pa.prev <- exp.table
  for (i in 1:dim(exp.table)[2])
  {
    unexp.wt.prev[,i] <- exp.prev[,i] <- exp.pa.prev[,i] <- 0
    unexp.wt.prev[round(exp.table[,i],3) < melbstreambiota::taxon.classes$pa.threshold[i],i] <- melbstreambiota::taxon.classes$pa.threshold[i] - exp.table[round(exp.table[,i],3) < melbstreambiota::taxon.classes$pa.threshold[i],i]
    #weight unexpected occurrences by pa.threshold minus probability of occurrence (so that more unexpected occurrences are given a higher weighting)
    exp.pa.prev[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i] <- 1
    exp.prev[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i] <- exp.table[round(exp.table[,i],3) >= melbstreambiota::taxon.classes$pa.threshold[i],i]
  }
  obs.wt.prev <- obs.wt*exp.pa.prev
  obs.wt.sensif.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(obs.wt.prev)))
  exp.wt.sensif.prev <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(exp.prev)))
  #observed weighted by nhi predictions and by SIGNAL grades
  obs.expdiff <- as.data.frame(t(round(t(obs.wt),3) - melbstreambiota::taxon.classes$pa.threshold))
  obs.expdiff <- obs.expdiff*obs.table
  # obs.expdiffWithKG08 <- as.data.frame(t(round(t(obs.wt),3) - melbstreambiota::taxon.classes$pa.threshold))*obs.wt
  # #KG08 is unexpected in all circumstances, so identify all observations of KG08 as -1 (and let the unexp grade take care of its weighting)
  #obs.expdiffWithKG08$KG08 <- -obs.table$KG08
  obs.expdiff$KG08 <- -obs.table$KG08
  obs.expdiffPos <- obs.expdiff#WithKG08
  obs.expdiffNeg <- obs.expdiff#WithKG08
  obs.expdiffPos[obs.expdiff < 0] <- 0  #WithKG08
  obs.expdiffNeg[obs.expdiff > 0] <- 0  #WithKG08
  #so this weights KG08 occurrences (negatively) by their likelihood, but still does not allow a positive effect of KG08 occurrence.
  expdiff.prev <- as.data.frame(t(round(t(exp.prev),3) - melbstreambiota::taxon.classes$pa.threshold))
  expdiff.prev <- expdiff.prev*exp.pa.prev #this ensures no negative numbers in this matrix
  expdiff.prev.sens <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(expdiff.prev)))
  obs.expdiffNeg.unexp <- as.data.frame(t(melbstreambiota::taxon.classes$unexp.grade*t(obs.expdiffNeg)))
  obs.expdiffPos.sensif <- as.data.frame(t(melbstreambiota::taxon.classes$sens.grade*t(obs.expdiffPos)))
  obs.NsensFams <- apply(obs.table[-c(weedy.index, invas.index)], 1, FUN = sum, na.rm = TRUE)
  obs.wt.sensif.prev.nonweed <- apply(obs.wt.sensif.prev[-c(weedy.index, invas.index)],
                                      1,FUN = sum, na.rm = TRUE)
  obs.expdiffPos.sensif.weedinvas <- apply(obs.expdiffPos.sensif[,c(weedy.index, invas.index)],
                                           1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.prev.nonweed <- apply(exp.wt.sensif.prev[-c(weedy.index, invas.index)],
                                      1,FUN = sum, na.rm = TRUE)
  expdiff.prev.sensif.weedinvas <- apply(expdiff.prev.sens[,c(weedy.index, invas.index)],
                                         1,FUN = sum, na.rm = TRUE)
  obs.expdiffNeg.unexp.weedinvas <- apply(obs.expdiffNeg.unexp[,c(weedy.index, invas.index)],
                                          1,FUN = sum, na.rm = TRUE)
  lumar <- data.frame(samppr = samppr,
                      lumar = (obs.wt.sensif.prev.nonweed + obs.expdiffPos.sensif.weedinvas) /
                        (exp.wt.sensif.prev.nonweed + expdiff.prev.sensif.weedinvas) + obs.expdiffNeg.unexp.weedinvas,
                      nSensFams = obs.NsensFams,
                      obs.wt.sensif.prev.nonweed = obs.wt.sensif.prev.nonweed,
                      obs.expdiffPos.sensif.weedinvas = obs.expdiffPos.sensif.weedinvas,
                      exp.wt.sensif.prev.nonweed = exp.wt.sensif.prev.nonweed,
                      expdiff.prev.sensif.weedinvas = expdiff.prev.sensif.weedinvas,
                      obs.expdiffNeg.unexp.weedinvas = obs.expdiffNeg.unexp.weedinvas,
                      obs.wt.A = apply(obs.wt.sensif.prev[melbstreambiota::taxon.classes$sens.gp == "A"],
                                       1,FUN = sum, na.rm = TRUE),
                      obs.wt.B = apply(obs.wt.sensif.prev[melbstreambiota::taxon.classes$sens.gp == "B"],
                                       1,FUN = sum, na.rm = TRUE),
                      obs.wt.C = apply(obs.wt.sensif.prev[melbstreambiota::taxon.classes$sens.gp == "C"],
                                       1,FUN = sum, na.rm = TRUE),
                      obs.wt.D = apply(obs.wt.sensif.prev[melbstreambiota::taxon.classes$sens.gp == "D"],
                                       1,FUN = sum, na.rm = TRUE),
                      obs.wt.weedy = apply(obs.wt.sensif.prev[melbstreambiota::taxon.classes$sens.gp == "weedy"],
                                           1,FUN = sum, na.rm = TRUE),
                      unexp.wt.weedy = apply(obs.expdiffNeg.unexp[,c(weedy.index)],
                                             1,FUN = sum, na.rm = TRUE),
                      unexp.wt.invas = obs.expdiffNeg.unexp[,c(invas.index)])
  lumar
}

#' All index variants considered by Walsh (2023)
#'
#'   Calculate all OE indices, and others, compared to LUMaR by Walsh (2023, https://osf.io/392kv). Note that SIGNAL indices
#'   need to be calculated using the full list of families collected in samples, not just the 59 families used in this
#'   function
#'
#' @param obs.table a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
#' Data for each samppr are 1s for observed presences and 0 for absences
#' @param exp.table a data.frame with the same structure as obs.table, but filled with probabilities of occurrence
#' as predicted by the 59 distribution models described by Walsh (2023) under no human impact
#' @param general.threshold if not NA, a single proportion signifiying the presence-absence threshold to be applied to all families
#' @return A data frame with the same number of rows as the input tables and 28 columns
#'#' \describe{
#'   \item{samppr}{\code{samppr} as in input tables}
#'   \item{OE50}{}
#'   \item{wOE50}{}
#'   \item{OE25}{}
#'   \item{wOE25}{}
#'   \item{OE.prev}{}
#'   \item{wOE.prev}{}
#'   \item{OE.pa.prev}{}
#'   \item{wOE.pa.prev}{}
#'   \item{wOE}{}
#'   \item{wOE.signal}{}
#'   \item{wOE.signal1}{}
#'   \item{wOE.signal.prev}{}
#'   \item{OE.signal.pa.prev}{}
#'   \item{simOE}{}
#'   \item{simOE.prev}{}
#'   \item{wOE.sensif}{}
#'   \item{wOE.sensif1}{}
#'   \item{wOE.sensif.prev}{}
#'   \item{OE.sensif.pa.prev}{}
#'   \item{wOE.sensif.minweed.prev}{}
#'   \item{wOE.sensif.minweed.prevA}{}
#'   \item{wOE.sensif.minweed.prevB}{}
#'   \item{wOE.sensif.minweed.prev1}{}
#'   \item{wOE.sensif.minweed.prev1A}{}
#'   \item{wOE.sensif.minweed.prev1B}{}
#'   \item{wOE.signal.minweed.prev}{}
#'   \item{wOE.signal.minweed.prev1}{}
#'   }
#' @seealso \code{\link{collateBugSamppr}}, \code{\link{predCurrNHI59}}, \code{\link{collateObsTable}}
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 2))
#' sampprs <- collateBugSamppr(sampprs)
#' expTable <- predCurrNHI59(sampprs)$predNHI
#' obsTable <-collateObsTable(bugData, sampprs)
#' oestats(obsTable[,-1], expTable[,-1])
#' @export
oestats <- function(obs.table, exp.table, general.threshold = NA)
{
  oe <- function(c.obs.exp)  #single vector concatenating obs and exp to allow apply
  {
    obs <- c.obs.exp[1:(0.5*length(c.obs.exp))]
    exp <- c.obs.exp[-(1:(0.5*length(c.obs.exp)))]
    #  oe <- sum(obs[exp > 0])/sum(exp)
    oe <- sum(obs)/sum(exp)
    oe
  }
  ordTaxon.classes <- melbstreambiota::taxon.classes[match(names(obs.table), melbstreambiota::taxon.classes$fam),]
  weedy.index <- which(names(obs.table) %in% ordTaxon.classes$fam[ordTaxon.classes$sens.gp == "weedy"] & names(obs.table) != "KG08")
  invas.index <- which(names(obs.table) == "KG08")
  weed.sens.grade <- unique(ordTaxon.classes$sens.grade[ordTaxon.classes$sens.gp == "weedy"])
  exp.table <- exp.table[match(row.names(obs.table), row.names(exp.table)),match(names(obs.table), names(exp.table))]
  #set expected occurrence of exotic Physidae to zero
  exp.table$KG08 <- 0
  if (!is.na(general.threshold))
    exp.table[exp.table < general.threshold] <- 0
  exp.temp <- exp.table
  exp.temp[exp.table >= 0.5] <- 1
  exp.temp[exp.table < 0.5] <- 0
  obs.wt <- obs.table*exp.table
  OE50 <- apply(cbind(obs.table, exp.temp), 1, FUN = oe)
  wOE50 <- apply(cbind(obs.wt, exp.temp), 1, FUN = oe)

  exp.temp <- exp.table
  exp.temp[exp.table >= 0.25] <- 1
  exp.temp[exp.table < 0.25] <- 0
  OE25 <- apply(cbind(obs.table, exp.temp), 1, FUN = oe)
  wOE25 <- apply(cbind(obs.wt, exp.temp), 1, FUN = oe)

  unexp.wt.prev <- unexp.pa.prev <- exp.prev <- exp.pa.prev <- exp.table
  for (i in 1:dim(exp.temp)[2])
  {
    unexp.wt.prev[,i] <- unexp.pa.prev[,i] <- exp.prev[,i] <- exp.pa.prev[,i] <- 0
    unexp.pa.prev[round(exp.table[,i],3) < ordTaxon.classes$pa.threshold[i],i] <- 1
    unexp.wt.prev[round(exp.table[,i],3) < ordTaxon.classes$pa.threshold[i],i] <-  ordTaxon.classes$pa.threshold[i] - exp.table[round(exp.table[,i],3) < ordTaxon.classes$pa.threshold[i],i]
    #weight unexpected occurrences by pa.threshold minus probability of occurrence (so that more unexpected occurrences are given a higher weighting)
    exp.pa.prev[round(exp.table[,i],3) >= ordTaxon.classes$pa.threshold[i],i] <- 1
    exp.prev[round(exp.table[,i],3) >= ordTaxon.classes$pa.threshold[i],i] <- exp.table[round(exp.table[,i],3) >= ordTaxon.classes$pa.threshold[i],i]
  }
  obs.prev <- obs.table*exp.pa.prev
  obs.wt.prev <- obs.wt*exp.pa.prev
  OE.prev <- apply(cbind(obs.prev, exp.prev), 1, FUN = oe)
  wOE.prev <- apply(cbind(obs.wt.prev, exp.prev), 1, FUN = oe)
  OE.pa.prev <- apply(cbind(obs.prev, exp.pa.prev), 1, FUN = oe)
  wOE.pa.prev <- apply(cbind(obs.wt.prev, exp.pa.prev), 1, FUN = oe)

  wOE <- apply(cbind(obs.table, exp.table), 1, FUN = oe)

  #observed weighted by nhi predictions and by SIGNAL grades
  obs.expdiff <- as.data.frame(t(round(t(obs.wt),3) - ordTaxon.classes$pa.threshold))
  obs.expdiff <- obs.expdiff*obs.table
  # obs.expdiffWithKG08 <- as.data.frame(t(round(t(obs.wt),3) - ordTaxon.classes$pa.threshold))*obs.wt
  # #KG08 is unexpected in all circumstances, so identify all observations of KG08 as -1 (and let the unexp grade take care of its weighting)
  # obs.expdiffWithKG08$KG08 <- -obs.table$KG08
  # obs.expdiffWithKG08 <- as.data.frame(t(round(t(obs.wt),3) - ordTaxon.classes$pa.threshold))*obs.wt
  # #KG08 is unexpected in all circumstances, so identify all observations of KG08 as -1 (and let the unexp grade take care of its weighting)
  #obs.expdiffWithKG08$KG08 <- -obs.table$KG08
  obs.expdiff$KG08 <- -obs.table$KG08
  obs.expdiffPos <- obs.expdiff#WithKG08
  obs.expdiffNeg <- obs.expdiff#WithKG08
  obs.expdiffPos[obs.expdiff < 0] <- 0  #WithKG08
  obs.expdiffNeg[obs.expdiff > 0] <- 0  #WithKG08
  #so this weights KG08 occurrences (negatively) by their likelihood, but still does not allow a positive effect of KG08 occurrence.
  expdiff.prev <- as.data.frame(t(round(t(exp.prev),3) - ordTaxon.classes$pa.threshold))
  expdiff.prev <- expdiff.prev*exp.pa.prev #this ensures no negative numbers in this matrix

  obs.signal <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(obs.table)))
  obs.signal.prev <- obs.signal*exp.pa.prev
  obs.wt.signal <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(obs.wt)))
  obs.wt.signal.prev <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(obs.wt.prev)))

  exp.wt.signal <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(exp.table)))
  exp.wt.signal.prev <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(exp.prev)))
  exp.signal.pa.prev <- as.data.frame(t(ordTaxon.classes$SIGNAL*t(exp.pa.prev)))

  obs.signal.prev.sum <- apply(obs.signal.prev,1,FUN = sum, na.rm = TRUE)
  obs.wt.signal.sum <- apply(obs.wt.signal,1,FUN = sum, na.rm = TRUE)
  obs.wt.signal.prev.sum <- apply(obs.wt.signal.prev,1,FUN = sum, na.rm = TRUE)
  exp.wt.signal.sum <- apply(exp.wt.signal,1,FUN = sum, na.rm = TRUE)
  exp.wt.signal.prev.sum <- apply(exp.wt.signal.prev,1,FUN = sum, na.rm = TRUE)
  exp.signal.pa.prev.sum <- apply(exp.signal.pa.prev,1,FUN = sum, na.rm = TRUE)

  wOE.signal <- obs.wt.signal.sum/exp.wt.signal.sum
  wOE.signal1 <- obs.wt.signal.sum/exp.wt.signal.prev.sum
  wOE.signal.prev <- obs.wt.signal.prev.sum/exp.wt.signal.prev.sum
  OE.signal.pa.prev <- obs.signal.prev.sum/exp.signal.pa.prev.sum

  obs.sensif <- as.data.frame(t(ordTaxon.classes$sens.grade*t(obs.table)))
  obs.sensif.prev <- obs.sensif*exp.pa.prev
  obs.wt.sensif <- as.data.frame(t(ordTaxon.classes$sens.grade*t(obs.wt)))
  obs.wt.sensif.prev <- as.data.frame(t(ordTaxon.classes$sens.grade*t(obs.wt.prev)))
  obs.expdiffPos.sensif <- as.data.frame(t(ordTaxon.classes$sens.grade*t(obs.expdiffPos)))

  exp.wt.sensif <- as.data.frame(t(ordTaxon.classes$sens.grade*t(exp.table)))
  exp.wt.sensif.prev <- as.data.frame(t(ordTaxon.classes$sens.grade*t(exp.prev)))
  exp.sensif.pa.prev <- as.data.frame(t(ordTaxon.classes$sens.grade*t(exp.pa.prev)))
  expdiff.prev.sens <- as.data.frame(t(ordTaxon.classes$sens.grade*t(expdiff.prev)))

  unexp.wt.unexpgd.prev <- as.data.frame(t(ordTaxon.classes$unexp.grade*t(unexp.wt.prev*obs.table)))
  obs.expdiffNeg.unexp <- as.data.frame(t(ordTaxon.classes$unexp.grade*t(obs.expdiffNeg)))

  obs.sensif.prev.sum <- apply(obs.sensif.prev,1,FUN = sum, na.rm = TRUE)
  obs.wt.sensif.sum <- apply(obs.wt.sensif,1,FUN = sum, na.rm = TRUE)
  obs.wt.sensif.prev.sum <- apply(obs.wt.sensif.prev,1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.sum <- apply(exp.wt.sensif,1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.prev.sum <- apply(exp.wt.sensif.prev,1,FUN = sum, na.rm = TRUE)
  exp.sensif.pa.prev.sum <- apply(exp.sensif.pa.prev,1,FUN = sum, na.rm = TRUE)

  wOE.sensif <- obs.wt.sensif.sum/exp.wt.sensif.sum
  wOE.sensif1 <- obs.wt.sensif.sum/exp.wt.sensif.prev.sum
  wOE.sensif.prev <- obs.wt.sensif.prev.sum/exp.wt.sensif.prev.sum
  OE.sensif.pa.prev <- obs.sensif.prev.sum/exp.sensif.pa.prev.sum

  simOE <- apply(cbind(exp.table, obs.table), 1, FUN = bray.2halves)
  simOE.prev <- apply(cbind(exp.prev, obs.wt.prev), 1, FUN = bray.2halves)
  simOE.pa.prev <- apply(cbind(exp.pa.prev, obs.prev), 1, FUN = bray.2halves)

  obs.sensif.prev.weed <- apply(obs.sensif.prev[,weedy.index],1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.weed <- apply(exp.wt.sensif[,weedy.index],1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.prev.weed <- apply(exp.wt.sensif.prev[,weedy.index],1,FUN = sum, na.rm = TRUE)
  exp.sensif.pa.prev.weed <- apply(exp.sensif.pa.prev[,weedy.index],1,FUN = sum, na.rm = TRUE)

  unexp.wt.unexpgd.prev.all <- apply(unexp.wt.unexpgd.prev,
                                     1,FUN = sum, na.rm = TRUE)
  unexp.wt.unexpgd.prev.weedinvas <- apply(unexp.wt.unexpgd.prev[,c(weedy.index, invas.index)],
                                           1,FUN = sum, na.rm = TRUE)
  obs.expdiffNeg.unexp.weedinvas <- apply(obs.expdiffNeg.unexp[,c(weedy.index, invas.index)],
                                          1,FUN = sum, na.rm = TRUE)
  obs.expdiffPos.sensif.weedinvas <- apply(obs.expdiffPos.sensif[,c(weedy.index, invas.index)],
                                           1,FUN = sum, na.rm = TRUE)
  expdiff.prev.sensif.weedinvas <- apply(expdiff.prev.sens[,c(weedy.index, invas.index)],
                                         1,FUN = sum, na.rm = TRUE)
  obs.sensif.prev.nonweed <- apply(obs.sensif.prev[-c(weedy.index, invas.index)],
                                   1,FUN = sum, na.rm = TRUE)
  obs.wt.sensif.nonweed <- apply(obs.wt.sensif[-c(weedy.index, invas.index)],
                                 1,FUN = sum, na.rm = TRUE)
  obs.wt.sensif.prev.nonweed <- apply(obs.wt.sensif.prev[-c(weedy.index, invas.index)],
                                      1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.nonweed <- apply(exp.wt.sensif[-c(weedy.index, invas.index)],
                                 1,FUN = sum, na.rm = TRUE)
  exp.wt.sensif.prev.nonweed <- apply(exp.wt.sensif.prev[-c(weedy.index, invas.index)],
                                      1,FUN = sum, na.rm = TRUE)
  exp.sensif.pa.prev.nonweed <- apply(exp.sensif.pa.prev[-c(weedy.index, invas.index)],
                                      1,FUN = sum, na.rm = TRUE)

  wOE.sensif.minweed.prev <-
    (obs.wt.sensif.prev.sum)/exp.wt.sensif.prev.sum  - unexp.wt.unexpgd.prev.all
  WOE.sensif.mindiffweed.prev <-
    (obs.wt.sensif.prev.nonweed + obs.expdiffPos.sensif.weedinvas) /
    (exp.wt.sensif.prev.nonweed + expdiff.prev.sensif.weedinvas) + obs.expdiffNeg.unexp.weedinvas
  # WOE.sensif.mindiffweed.prev was  best performing index and renamed LUMaR
  wOE.signal.minweed.prev <- (0.1*obs.wt.signal.prev.sum)/(0.1*exp.wt.signal.prev.sum)  - unexp.wt.unexpgd.prev.all
  wOE.signal.minweed.prev1 <- (0.1*obs.wt.signal.sum)/(0.1*exp.wt.signal.prev.sum) - unexp.wt.unexpgd.prev.all
  stats.df <- data.frame(samppr = row.names(obs.table),
                         OE50 = OE50,
                         wOE50 = wOE50,
                         OE25 = OE25,
                         wOE25 = wOE25,
                         OE.prev = OE.prev,
                         wOE.prev = wOE.prev,
                         OE.pa.prev = OE.pa.prev,
                         wOE.pa.prev = wOE.pa.prev,
                         wOE = wOE,
                         wOE.signal = wOE.signal,
                         wOE.signal1 = wOE.signal1,
                         wOE.signal.prev =  wOE.signal.prev,
                         OE.signal.pa.prev =  OE.signal.pa.prev,
                         simOE = simOE,
                         simOE.prev = simOE.prev,
                         wOE.sensif = wOE.sensif,
                         wOE.sensif1 = wOE.sensif1,
                         wOE.sensif.prev = wOE.sensif.prev,
                         OE.sensif.pa.prev = OE.sensif.pa.prev,
                         wOE.sensif.minweed.prev = wOE.sensif.minweed.prev,  #21
                         WOE.sensif.mindiffweed.prev = WOE.sensif.mindiffweed.prev,  # = LUMaR!
                         wOE.signal.minweed.prev = wOE.signal.minweed.prev,
                         wOE.signal.minweed.prev1 = wOE.signal.minweed.prev1)
  stats.df
}

#' Calculate SIGNAL and SIGNAL2 scores from macroinvertebrate data
#'
#' Collates macroinvertebrate data (in two possible formats) and returns SIGNAL and
#' SIGNAL2 scores for each sample.
#'
#' @param bugData Table with either A) 2 columns (\code{samppr}, \code{bugcode}), or A) one row per samppr,
#' with the first column \code{samppr} and the remaining column headings \code{bugcode}s
#' @return  A data.frame with 3 columns: samppr, matching sampprs$samppr; SIGNAL and SIGNAL2.
#' @details While all other functions in this package require sample-pairs of rapid bioassessment
#' samples, this function makes no assumption. SIGNAL scores can be calculated for any sample method.
#' All bugcodes should be consistent with taxon.classes. See \url{http://www.mdfrc.org.au/bugguide/resources/AUSRIVAS_Taxacodes.pdf}
#' for a full list including taxa other than those in taxon.classes.  Note that the convention used in this package is to
#' omit the trailing '9999' from each code. If bugcodes in bugData include trailing 4 digits, these are removed
#' (this effectively combines any lower-taxonomic-level taxa to family-level.)
#' SIGNAL is calculated using all taxa listed in bugData assigned a score in
#' EPA Victoria (2003) Rapid bioassessment methodology for rivers and streams.
#' Report 604.1. Environment Protection Authority Victoria, Melbourne.  SIGNAL2 uses
#' the scores listed by Chessman, B.C. (2003) New sensitivity grades for Australian
#' river macroinvertebrates. Marine and Freshwater Research, 54, 95–103.
#' @examples
#' #Data in Form A
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 2))
#' #When importing data, replace the system.file() statement with "path/excelFile.xls"
#' signalScores1 <- calcSIGNAL(bugData)
#' #Data in Form B
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package = "melbstreambiota", mustWork = TRUE),
#'                                         sheet = 3))
#' signalScores2 <- calcSIGNAL(bugData)
#' #Check that data imported in both forms are equql
#' sum(signalScores1[-1] - signalScores2[-1])  # = 0
#' @export
calcSIGNAL <- function(bugData) {
  untab <- function(x, rows = "sample", cols = "bugcode"){
    untabx <- data.frame(a = NULL,b = NULL,abundance = NULL)
    for (i in 2:length(names(x))) {
      untabx <- rbind(untabx,
                      data.frame(a = x[,1],
                                 b = names(x)[i],
                                 abundance = x[,i]))
    }
    untabx <- untabx[untabx$abundance > 0,]
    names(untabx) <- c(rows,cols,"count")
    untabx <- untabx[order(untabx[,1]),]
    untabx
  }
  if (sum(names(bugData) %in% c("samppr","bugcode")) == 2)
  {
    bugData$samppr <- as.vector(bugData$samppr)
    bugData <- as.data.frame.matrix(table(bugData[c("samppr","bugcode")]))
    bugData <- data.frame(samppr = row.names(bugData), bugData)
    row.names(bugData) <- 1:length(bugData$samppr)
  }
  bugsList <- untab(bugData, rows = "samppr", cols = "bugcode")
  bugsList <- bugsList[order(bugsList$samppr), c("samppr","bugcode","count")]
  bugsList <- unique(bugsList[,1:2])
  #shorten any long bugcodes
  bugsList$bugcode <- substr(bugsList$bugcode,1,4)
  bugsList$SIGNAL <- melbstreambiota::bugfams$SIGNALWoV2003[match(bugsList$bugcode, melbstreambiota::bugfams$bugcode)]
  bugsList$SIGNAL2 <- melbstreambiota::bugfams$SIGNAL2[match(bugsList$bugcode, melbstreambiota::bugfams$bugcode)]
  sampprs <- stats::aggregate(bugsList[c("SIGNAL","SIGNAL2")], by = list(samppr = bugsList$samppr),
                       FUN = mean, na.rm = TRUE)
  sampprs
}

