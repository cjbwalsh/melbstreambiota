#' Collate and format sample-pair data for applying macroinvertebrate model predictions
#'
#' @param sampprTable Table containing at least the fields \code{subc}, \code{nspring},\code{nriff},
#' and either \code{process} or \code{processN}. If the table also contains a samppr identification column,
#' it should be called 'samppr'
#' @param AttImp_L9 either the default value or a vector of new values for all subcs, or a single value if
#' all sampprs are to have the same value
#' @param AttForest_L35W1000 either the default value or a vector of new values for all subcs, or a single value if
#' all sampprs are to have the same value
#' @param meanAnnQ_mm either the default value or a vector of new values for all subcs
#' @param mnAnnAirTm_deg either the default values, or a single number, added to
#' mwstreams$mnAnnAirTm_deg (e.g. 1 = 1 degrees warmer)
#' @param SRI_48mth_weighted if default, a "date" field must be supplied, or if a vector
#' of length dim(sampprTable)[1], then those values are used
#' @return  The original \code{sampprTable} with additional columns required for model predictions
#' @details If arguments \code{AttImp_L9}, \code{AttForest_L35W1000}, \code{meanAnnQ_mm}, \code{mnAnnAirTm_deg},
#' are default values, then 2006 values are drawn from the mwstreams table, matched by \code{subc}.
#' If \code{SRI_48mth_weighted} is the default, values are drawn from \code{sri48moW}, matched by  \code{subc}
#' and  \code{date}.  Alternatively, for these arguments, vectors of length dim(sampprTable)[1]
#' can be supplied to represent scenarios other than 2006 conditions.  If supplying such data, care should be
#' taken not to set values outside the experience of the model (see Walsh and Webb 2014).  For instance, the
#' maximum air temperature value in the experience of the models is 15.3, and setting mnAnnAirTm_deg to 2 (i.e.
#' adding 2 degrees to the long-term average) raises the mean air temperature to >15.3 for many lowland reaches.
#' @examples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 1))
#' #When importing data, replace the system.file() statement with "path/excelFile.xls"
#' sampprs1 <- collateBugSamppr(sampprs)
#' @export
collateBugSamppr <- function(sampprTable,
                             AttImp_L9 = "current (2006)",
                             AttForest_L35W1000 = "current (2006)",
                             meanAnnQ_mm = "current (2006)",
                             mnAnnAirTm_deg = "long-term",
                             SRI_48mth_weighted = "sample date") {
  ##Begin by checking data validity
  if (sum(c("subc","nspring","nriff") %in% names(sampprTable)) < 3)
    stop("sampprTable must contain the fields 'subc','nspring','nriff'", call. = FALSE)
  if (sum(c("process","processN") %in% names(sampprTable)) > 0)
  {
    if ("processN" %in% names(sampprTable))
    {
      if (sum(is.na(match(sampprTable$processN, c(0,1)))) == 0)
      {
        warning("The field processN will be used as 1 = 'field-sort', 0 = 'lab-sort'", call. = FALSE)
      }else{
        stop("processN can only = 1 (field-sort) or 2 (lab-sort)", call. = FALSE)
      }
    }
      if ("process" %in% names(sampprTable) & !"processN" %in% names(sampprTable))
      {
        if (sum(is.na(match(sampprTable$process, c("lab-sort","field-sort")))) > 0)
        {
          stop("process can only = 'field-sort or 'lab-sort'", call. = FALSE)
        } else {
          sampprTable$processN <- 0
          sampprTable$processN[sampprTable$process == "field-sort"] <- 1
        }
      }
    }
  if (sum(!sampprTable$subc %in% melbstreambiota::mwstreams$subc) > 0)
  {
    subc_problems <- which(!sampprTable$subc %in% melbstreambiota::mwstreams$subc)
    warning(cat("The following subc values are in the input sampprTable, but are not in the mwstreams
table and the", length(subc_problems),"sampprs with those values were excluded from the analysis.
Check for typos or check their location in GIS. \n",
                  paste(unique(sampprTable$subc[subc_problems]),"\n")),
            call. = FALSE)
    sampprTable <- sampprTable[!subc_problems,]
    if(dim(sampprTable)[1] == 0)
      stop("None of the supplied subc values match values in the mwstreams table.  Check your inputs \n")
  }
  # if (sum(!sampprTable$segmentno %in% melbstreambiota::mwstreams$segmentno) > 0)
    #   {
    #     problems1 <- which(!sampprTable$segmentno %in% melbstreambiota::mwstreams$segmento)
    #     problems1 <- problems1[!problems1 %in% problems]
    #     warning(paste("The following entries have no matching segmentno in melbstreambiota::mwstreams and were excluded from the analysis.
    #                   Check their location using quickzMelMap() or in GIS, and then the segmentno in melbstreambiota::mwstreams.",
    #                   sampprTable$subc[problems1]),
    #             call. = FALSE)
    # }
    if (sum(is.na(match(sampprTable$nspring, c(0,1,2)))) +
        sum(is.na(match(sampprTable$nriff, c(0,1,2)))) > 0)
    {
      stop("nspring and nriff can only equal 0, 1 or 2. Please correct before proceeding.", call. = FALSE)
    }
    if (AttImp_L9[1] == "current (2006)")
    {
      sampprTable$AttImp_L9 <- melbstreambiota::mwstreams$AttImp_L9[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
    }else{
#      if (length(AttImp_L9) == 1) {
        sampprTable$AttImp_L9 <- AttImp_L9
#      }else{
#      sampprTable$AttImp_L9 <- sampprTable$AttImp_L9
#      }
    }
    if (AttForest_L35W1000[1] == "current (2006)")
    {
      sampprTable$AttForest_L35W1000 <- melbstreambiota::mwstreams$AttForest_L35W1000[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
    }else{
#      if (length(AttForest_L35W1000) == 1) {
        sampprTable$AttForest_L35W1000 <- AttForest_L35W1000
#      }else{
#        sampprTable$AttForest_L35W1000 <- sampprTable$AttForest_L35W1000
#      }
    }
  sampprTable$CatchmentArea_km2_InclDams <- melbstreambiota::mwstreams$CatchmentArea_km2_InclDams[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
  if (meanAnnQ_mm[1] == "current (2006)")
  {
    sampprTable$meanAnnQ_mm <- melbstreambiota::mwstreams$meanAnnQ_mm[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    sampprTable$meanAnnQ_mm <- sampprTable$meanAnnQ_mm
  }
  if (mnAnnAirTm_deg == "long-term")
  {
    sampprTable$mnAnnAirTm_deg <- melbstreambiota::mwstreams$mnAnnAirTm_deg[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    sampprTable$mnAnnAirTm_deg <- melbstreambiota::mwstreams$mnAnnAirTm_deg[match(sampprTable$subc,melbstreambiota::mwstreams$subc)] + mnAnnAirTm_deg
  }
  sampprTable$CatIgneous <- melbstreambiota::mwstreams$CatIgneous[match(sampprTable$subc,melbstreambiota::mwstreams$subc)]
  if (SRI_48mth_weighted[1] == "sample date")
  {
    sampprTable$SRI_48mth_weighted <- NA
    if (!"date" %in% names(sampprTable))
      stop("No date field: this is necessary if SRI_48mth_weighted is to be calculated", call. = FALSE)
    if (is.factor(sampprTable$date)) sampprTable$date <- as.vector(sampprTable$date)
    if (is.numeric(sampprTable$date))
      stop("date field is numeric. Please convert to date format.
           If importing from Excel, try saving as an excel file and using read_excel() from package readxl to import.",
           call. = FALSE)
    if (is.character(sampprTable$date))
      stop("date field is a character vector. Please convert to date format.
           Try using one of the functions dmy(), mdy(), or ymd() etc from package lubridate to save them as dates",
           call. = FALSE)
    if (lubridate::is.Date(sampprTable$date))
    {
      sampprTable$date <- as.POSIXct(sampprTable$date)
      sampprTable$date <- lubridate::force_tz(sampprTable$date, tzone = "UTC")
    }
    if (!lubridate::is.POSIXct(sampprTable$date)) {
      stop("date is not in POSIXct format.  Convert or import data data from excel using read_excel()", call. = FALSE)
    }
    if (lubridate::tz(sampprTable$date) != "UTC") {
      sampprTable$date <- lubridate::force_tz(sampprTable$date, tzone = "UTC")
      warning("Time zone of sampprTable$date converted to UTC. This only potentially matters if date values are not whole days",
              call. = FALSE)
    }
    if (sum(is.na(sampprTable$date)) > 0) {
      warning(paste(sum(is.na(sampprTable$date)),
                    "rows have date values that could not be coverted to a date.
                    For these rows SRI_48mth_weighted was set to 1 (average antecedent conditions)"), call. = FALSE)
    }
    dateErrors <- data.frame(index = NULL, problem = NULL)
    for (i in 1:dim(sampprTable)[1]) {
      srii <- melbstreambiota::sri48moW$SRI_48mth_weighted[melbstreambiota::sri48moW$date == lubridate::floor_date(sampprTable$date[i],"month") &
                                            melbstreambiota::sri48moW$subc == sampprTable$subc[i]]
      if (length(srii) == 0) {
        lastDate <- max(melbstreambiota::sri48moW$date[melbstreambiota::sri48moW$subc == sampprTable$subc[i]])
        lastSRI <- utils::tail(melbstreambiota::sri48moW$SRI_48mth_weighted[melbstreambiota::sri48moW$subc == sampprTable$subc[i]],1)
        dateErrors <- rbind(dateErrors, data.frame(index = i,
                                                   problem = ifelse(is.na(sampprTable$date[i]),1,
                                                                    ifelse(sampprTable$date[i] > lastDate,
                                                                           2, 3))))
        srii <- ifelse(is.na(sampprTable$date[i]),
                       1, ifelse(sampprTable$date[i] > lastDate, lastSRI, NA))
      }
      sampprTable$SRI_48mth_weighted[i] <- srii
    }
    if (sum(dateErrors$problem == 1) > 1)
      warning(sum(dateErrors$problem == 1), "sampprs were missing dates
              Reported results likely to be unreliable for these sampprs: \n",
              paste(sampprTable$samppr[dateErrors$index[dateErrors$problem == 1]], collapse = ",\n"))
    if (sum(dateErrors$problem == 2) > 1)
      warning(sum(dateErrors$problem == 2), " dates were more recent than antecedent flow data.  SRI_48mth_weighted was set at the latest available record.
Reported results likely to be unreliable for sampprs: \n",
    paste(sampprTable$samppr[dateErrors$index[dateErrors$problem == 2]], collapse = ",\n"))
    if (sum(dateErrors$problem == 3) > 1)
      warning(sum(dateErrors$problem == 3), "sampprs had undiagnosed date problems: ,
              SRI_48mth_weighted set to NA for these sampprs: \n",
              paste(sampprTable$samppr[dateErrors$index[dateErrors$problem == 3]], collapse = ",\n"))
    }else{
      sampprTable$SRI_48mth_weighted <- SRI_48mth_weighted
      warning("Supplied SRI_48mth_weighted values used", call. = FALSE)
    }
  if(sum("samppr" %in% names(sampprTable)) == 1){
sampprsOut <- sampprTable[c("samppr","subc","AttImp_L9","AttForest_L35W1000","meanAnnQ_mm","CatchmentArea_km2_InclDams",
                "mnAnnAirTm_deg","CatIgneous","SRI_48mth_weighted", "nspring", "nriff", "processN")]
  }else{
sampprsOut <- sampprTable[c("subc","AttImp_L9","AttForest_L35W1000","meanAnnQ_mm","CatchmentArea_km2_InclDams",
                  "mnAnnAirTm_deg","CatIgneous","SRI_48mth_weighted", "nspring", "nriff", "processN")]
  }
  sampprsOut
}

#' Collate and format sample data for applying fish or platypus model predictions
#'
#' @param sampleTable Table containing at least the field \code{subc}
#' @param FishOrPlatypus Predictor variables to be assembled for "fish" (the default) or "platypus"
#' @param AttImp_L9 either the default value or a vector of new values for all subcs, or a single value if
#' all sampprs are to have the same value. If FishOrPlatypus = "platypus", AttImpMin4k_L9 is substituted, and the
#' field AttImpMin4k_L9.
#' @param AttForest_L35W1000 either the default value or a vector of new values for all subcs, or a single value if
#' all sampprs are to have the same value
#' @param meanAnnQ_mm either the default value or a vector of new values for all subcs
#' @param mnAnnAirTm_deg either the default values, or a single number, added to
#' mwstreams$mnAnnAirTm_deg (e.g. 1 = 1 degrees warmer)
#' @param SRI_48mth_weighted if default, a "date" field must be supplied, or if a vector
#' of length dim(sampleTable)[1], then those values are used
#' @param barriersFromYear, if TRUE and FishOrPlatypus = "fish", a "date" field must be supplied,
#' if FALSE values must be provided for nPartBarriersDS, nFullBarriersDS
#' @param PartBarriersDS Only use if barriersFromYear is FALSE and FishOrPlatypus = "fish".
#' Must be a single numeric value (i.e. a number of barriers) to be applied ot all samples, or
#' a vector of values of length(SampleTable$subc) to be applied to each sample.
#' @param FullBarriersDS As for nPartBarriersDS
#' @return  The original \code{sampleTable} with additional columns required for model predictions
#' @details If arguments \code{AttImp_L9}, \code{AttForest_L35W1000}, \code{meanAnnQ_mm}, \code{mnAnnAirTm_deg},
#' are default values, then 2006 values are drawn from the mwstreams table, matched by \code{subc}.
#' If \code{SRI_48mth_weighted} is the default, values are drawn from \code{sri48moW}, matched by  \code{subc}
#' and\code{date}.  If \code{barriers} is the default, then "nPartBarriersDS" and
#' "nFullBarriersDS" are drawn from the barriers fields in mwstreams that match the year in which
#' each sample was collected (as determined by  \code{date}.  Alternatively, for these arguments, vectors of length dim(sampleTable)[1]
#' can be supplied to represent scenarios other than 2006 conditions.  If supplying such data, care should be
#' taken not to set values outside the experience of the model (see Walsh and Webb 2014).  For instance, the
#' maximum air temperature value in the experience of the models is 15.3, and setting mnAnnAirTm_deg to 2 (i.e.
#' adding 2 degrees to the long-term average) raises the mean air temperature to >15.3 for many lowland reaches.
#' @examples
#' # A sample table that would be used for fish samples taken on the same day as the example
#' # macroinvertebrate samples
#' sampprs <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                          package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 1))
#' #When importing data, replace the system.file() statement with "path/excelFile.xls"
#' sampprs1 <- collateSampleFP(sampprs)
#' #A hypothetical sample from a subc in the mwstreams table (average antecedent
#' #flow, all fish barriers removed)
#' exampleFishSamp <- collateSampleFP(sampleTable = mwstreams[1,],
#'                                    FishOrPlatypus = "fish",
#'                                    SRI_48mth_weighted = 1,
#'                                    barriersFromYear = FALSE,
#'                                    PartBarriersDS = 0, FullBarriersDS = 0)
#' exampleFishSamp
#' @export
collateSampleFP <- function(sampleTable,
                              FishOrPlatypus = "fish",
                              AttImp_L9 = "current (2006)",
                              AttForest_L35W1000 = "current (2006)",
                              meanAnnQ_mm = "current (2006)",
                              mnAnnAirTm_deg = "long-term",
                              SRI_48mth_weighted = "sample date",
                              barriersFromYear = TRUE,
                              PartBarriersDS = NA,
                              FullBarriersDS =NA) {
  ##Begin by checking data validity
  if (sum(c("subc") %in% names(sampleTable)) == 0)
    stop("sampleTable must contain at least the field 'subc'", call. = FALSE)
  FishOrPlatypus <- tolower(FishOrPlatypus)
  if(sum(c("fish","platypus") %in% FishOrPlatypus) != 1)
    stop("FishOrPlatypus must equal one of 'fish' or 'platypus'", call. = FALSE)
  if (sum(!sampleTable$subc %in% melbstreambiota::mwstreams$subc) > 0)
  {
    subc_problems <- which(!sampleTable$subc %in% melbstreambiota::mwstreams$subc)
    warning(cat("The following subc values are in the input sampleTable, but are not in the mwstreams
table and the", length(subc_problems),"sampprs with those values were excluded from the analysis.
Check for typos or check their location in GIS. \n",
                paste(unique(sampleTable$subc[subc_problems]),"\n")),
            call. = FALSE)
    sampleTable <- sampleTable[!subc_problems,]
    if(dim(sampleTable)[1] == 0)
    stop("None of the supplied subc values match values in the mwstreams table.  Check your inputs \n")
  }
  if(FishOrPlatypus == "fish"){
  if (AttImp_L9[1] == "current (2006)")
  {
    sampleTable$AttImp_L9 <- melbstreambiota::mwstreams$AttImp_L9[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    sampleTable$AttImp_L9 <- AttImp_L9
  }
  }else{
    if (AttImp_L9[1] == "current (2006)")
    {
      sampleTable$AttImpMin4k_L9 <- melbstreambiota::mwstreams$AttImpMin4k_L9[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
    }else{
      sampleTable$AttImpMin4k_L9 <- AttImp_L9
    }
  }
  if (AttForest_L35W1000[1] == "current (2006)")
  {
    sampleTable$AttForest_L35W1000 <- melbstreambiota::mwstreams$AttForest_L35W1000[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    #      if (length(AttForest_L35W1000) == 1) {
    sampleTable$AttForest_L35W1000 <- AttForest_L35W1000
    #      }else{
    #        sampleTable$AttForest_L35W1000 <- sampleTable$AttForest_L35W1000
    #      }
  }
  sampleTable$CatchmentArea_km2_InclDams <- melbstreambiota::mwstreams$CatchmentArea_km2_InclDams[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  if (meanAnnQ_mm[1] == "current (2006)")
  {
    sampleTable$meanAnnQ_mm <- melbstreambiota::mwstreams$meanAnnQ_mm[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    sampleTable$meanAnnQ_mm <- sampleTable$meanAnnQ_mm
  }
  if (mnAnnAirTm_deg == "long-term")
  {
    sampleTable$mnAnnAirTm_deg <- melbstreambiota::mwstreams$mnAnnAirTm_deg[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  }else{
    sampleTable$mnAnnAirTm_deg <- melbstreambiota::mwstreams$mnAnnAirTm_deg[match(sampleTable$subc,melbstreambiota::mwstreams$subc)] + mnAnnAirTm_deg
  }
  sampleTable$CatIgneous <- melbstreambiota::mwstreams$CatIgneous[match(sampleTable$subc,melbstreambiota::mwstreams$subc)]
  if (tolower(SRI_48mth_weighted[1]) == "sample date" | (barriersFromYear & FishOrPlatypus == "fish"))
  {
    if(tolower(SRI_48mth_weighted[1]) == "sample date"){
    sampleTable$SRI_48mth_weighted <- NA
    if (!"date" %in% names(sampleTable))
      stop("No date field: this is necessary if SRI_48mth_weighted is to be calculated", call. = FALSE)
    }
    if(barriersFromYear & FishOrPlatypus == "fish"){
      sampleTable$PartBarriersDS <- sampleTable$FullBarriersDS <- NA
      if (!"date" %in% names(sampleTable))
        stop("No date field: this is necessary if nPartBarriersDS and nFullBarriersDS are to be assigned", call. = FALSE)
    }
    if (is.factor(sampleTable$date)) sampleTable$date <- as.vector(sampleTable$date)
    if (is.numeric(sampleTable$date))
      stop("date field is numeric. Please convert to date format.
           If importing from Excel, try saving as an excel file and using read_excel() from package readxl to import.",
           call. = FALSE)
    if (is.character(sampleTable$date))
      stop("date field is a character vector. Please convert to date format.
           Try using one of the functions dmy(), mdy(), or ymd() etc from package lubridate to save them as dates",
           call. = FALSE)
    if (lubridate::is.Date(sampleTable$date))
    {
      sampleTable$date <- as.POSIXct(sampleTable$date)
      sampleTable$date <- lubridate::force_tz(sampleTable$date, tzone = "UTC")
    }
    if (!lubridate::is.POSIXct(sampleTable$date)) {
      stop("date is not in POSIXct format.  Convert or import data from excel using read_excel()", call. = FALSE)
    }
    if (lubridate::tz(sampleTable$date) != "UTC") {
      sampleTable$date <- lubridate::force_tz(sampleTable$date, tzone = "UTC")
      warning("Time zone of sampleTable$date converted to UTC. This only potentially matters if date values are not whole days",
              call. = FALSE)
    }
    if (sum(is.na(sampleTable$date)) > 0) {
      warning(paste(sum(is.na(sampleTable$date)),
                    "rows have date values that could not be coverted to a date.
                    For these rows SRI_48mth_weighted was set to 1 (average antecedent conditions) and
                    (if FIshOrPlatypus = 'fish')
                    nPartBarriers and NFullBarriers were set to 2016 values"), call. = FALSE)
    }
    dateErrors <- data.frame(index = NULL, problem = NULL)
    if(tolower(SRI_48mth_weighted[1]) == "sample date"){
      for (i in 1:dim(sampleTable)[1]) {
      srii <- melbstreambiota::sri48moW$SRI_48mth_weighted[melbstreambiota::sri48moW$date == lubridate::floor_date(sampleTable$date[i],"month") &
                                                            melbstreambiota::sri48moW$subc == sampleTable$subc[i]]
      if(sum(melbstreambiota::sri48moW$subc == sampleTable$subc[i]) == 0)
        subcErrors <- rbind()
      if (length(srii) == 0) {
        lastDate <- max(melbstreambiota::sri48moW$date[melbstreambiota::sri48moW$subc == sampleTable$subc[i]])
        lastSRI <- utils::tail(melbstreambiota::sri48moW$SRI_48mth_weighted[melbstreambiota::sri48moW$subc == sampleTable$subc[i]],1)
        dateErrors <- rbind(dateErrors, data.frame(index = i,
                                                   problem = ifelse(is.na(sampleTable$date[i]),1,
                                                                    ifelse(sampleTable$date[i] > lastDate,
                                                                           2, 3))))
        srii <- ifelse(is.na(sampleTable$date[i]),
                       1, ifelse(sampleTable$date[i] > lastDate, lastSRI, NA))
      }
      sampleTable$SRI_48mth_weighted[i] <- srii
      }
    }
      for (i in 1:dim(sampleTable)[1]) {
        yeari <- ifelse(is.na(sampleTable$date[i]),
                        2016, min(2016,lubridate::year(sampleTable$date[i])))
        #find nearest matching partBarrier year
        pyeari <- c(1996,1997,1999,2000,2002,2004,2005,2006,2007,2008,2009,2010,2016)[which.min(abs(c(1996,1997,1999,2000,2002,2004,2005,2006,2007,2008,2009,2010,2016) - yeari))]
        #find nearest matching fullBarrier year
        fyeari <- c(2006,2007,2008,2009,2012,2014)[which.min(abs(c(2006,2007,2008,2009,2012,2014) - yeari))]
        fyeari <- ifelse(fyeari == 1996, "pre1997", fyeari)
        pyeari <- ifelse(pyeari == 2006, "pre2007", pyeari)
        sampleTable$PartBarriersDS[i] <- melbstreambiota::mwstreams[melbstreambiota::mwstreams$subc == sampleTable$subc[i],
                                          paste("nPartBarriersDS_",pyeari,sep = "")]
        sampleTable$FullBarriersDS[i] <- melbstreambiota::mwstreams[melbstreambiota::mwstreams$subc == sampleTable$subc[i],
                                          paste("nFullBarriersDS_",fyeari,sep = "")]
      }
    if (sum(dateErrors$problem == 1) > 1)
      warning(sum(dateErrors$problem == 1), "sampprs were missing dates
              Reported results likely to be unreliable for these sampprs: \n",
              paste(sampleTable$samppr[dateErrors$index[dateErrors$problem == 1]], collapse = ",\n"))
    if (sum(dateErrors$problem == 2) > 1)
      warning(sum(dateErrors$problem == 2), " dates were more recent than antecedent flow data.  SRI_48mth_weighted was set at the latest available record.
              Reported results likely to be unreliable for sampprs: \n",
              paste(sampleTable$samppr[dateErrors$index[dateErrors$problem == 2]], collapse = ",\n"))
    if (sum(dateErrors$problem == 3) > 1)
      warning(sum(dateErrors$problem == 3), "sampprs had undiagnosed date problems: ,
              SRI_48mth_weighted set to NA for these sampprs: \n",
              paste(sampleTable$samppr[dateErrors$index[dateErrors$problem == 3]], collapse = ",\n"))
    }else{
      sampleTable$SRI_48mth_weighted <- SRI_48mth_weighted
      warning("Supplied SRI_48mth_weighted values used", call. = FALSE)
      if (!barriersFromYear & FishOrPlatypus == "fish"){
        if (is.na(PartBarriersDS[1]) | is.na(FullBarriersDS[1]))
          stop("barriersFromYear is FALSE: values must be supplied for PartBarriersDS and FullBarriersDS", call. = FALSE)
        if (!length(PartBarriersDS) %in% c(1, dim(sampleTable)[1]) |
            !length(FullBarriersDS) %in% c(1, dim(sampleTable)[1]))
          stop("Values of PartBarriersDS and FullBarriersDS must be either a single integer, or a
               vector of integers of length(sampleTable$subc)", call. = FALSE)
        sampleTable$PartBarriersDS <- PartBarriersDS
        sampleTable$FullBarriersDS <- FullBarriersDS
        warning("Supplied values used for PartBarriersDS and FullBarriersDS", call. = FALSE)
      }
    }
  if (FishOrPlatypus == "fish"){
    ###Note: we need to standardize predictor names: I think the following substitutions are correct
  sampleTable <- sampleTable[c("subc","AttImp_L9","AttForest_L35W1000","meanAnnQ_mm","CatchmentArea_km2_InclDams",
                             "mnAnnAirTm_deg","CatIgneous","SRI_48mth_weighted", "PartBarriersDS", "FullBarriersDS")]
  }else{
    if(tolower(SRI_48mth_weighted[1]) != "sample date")
    sampleTable <- sampleTable[c("subc","AttImpMin4k_L9","AFb10L1000","meanAnnQ_mm","CatchmentArea_km2_InclDams",
                               "mnAnnAirTm_deg","CatIgneous","SRI_48mth_weighted", "LWDBank", "vegBank")]
  }
  names(sampleTable)[names(sampleTable) %in% c("SRI_48mth_weighted")]  <-   c("SRI_48_triang")
  sampleTable
}


#' Collate and format observed table for LUMaR calculation from macroinvertebrate data
#'
#' Collates macroinvertebrate data (in two possible formats) into an observed family-by-sample-pair
#' matrix for calculation of LUMar (and other indices).  The function also compares the macroinvertebrate
#' data to the sample-pair table to ensure consistency.
#'
#' @param bugData Table with either A) 2 columns (\code{samppr}, \code{bugcode}), or A) one row per sample pair,
#' with the first column \code{samppr} and the remaining column headings \code{bugcode}s
#' @param sampprTable Table containing at least the fields \code{subc}, \code{nspring},\code{nriff},
#' and either \code{process} or \code{processN} (see \code{\link{collateBugSamppr}})
#' @return  A data.frame with 56 columns, the first samppr, matching sampprs$samppr, and the rest with names
#' equal to \code{taxon.classes$fam}. Presence are recorded as 1s and absences as 0s.
#' @details The vector of unique \code{samppr}s in bugData must completely match \code{sampprTable$samppr}. If bugData is in form A,
#' it is assumed that any listed family is present in the sample-pair (i.e. there are no taxa listed with abundance  = 0).
#' If there are duplicate entries of a family in a sample-pair, it is recorded as a single presence.  If bugData is in
#' form B, any family other than the 59 \code{taxon.classes$fam}s are removed, and any of the 59 that are missing are inserted
#' with abundance zero.
#' All bugcodes should be consistent with taxon.classes. See \url{http://www.mdfrc.org.au/bugguide/resources/AUSRIVAS_Taxacodes.pdf}
#' for a full list including taxa other than those in taxon.classes.  Note that the convention used in this package is to
#' omit the trailing 9999 from each code. If bugcodes in bugData include trailing 4 digits, these are removed
#' (this effectively combines any lower-taxonomic-level taxa to family-level.)
#' Taxonomic conventions used in the models are standard family-level with the following exceptions:
#' Chironomidae are identified to sub-family. The tribes of sub-family Chironominae ("QDAG","QDAH","QDAI") are combined to "QDAJ",
#' 'Old' odonate taxonomy is used: new families split from Corduliidae ("QO23", "QO26", "QO28", "QO27", "QO30")
#' are consolidated to "QO16", and two families formerly considered Aeshnidae ("QO21", "QO12") are combined as "QO12".
#' If bugData contains any of these family, this function converts them as above.
#' @examples
#' #Data in Form A
#' sampprTable <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                     package="melbstreambiota", mustWork = TRUE),
#'                                        sheet = 1))
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package="melbstreambiota", mustWork = TRUE),
#'                                         sheet = 2))
#' #When importing data, replace the system.file() statement with "path/excelFile.xls"
#' obsTable1 <- collateObsTable(bugData, sampprTable)
#' #Data in Form B
#' bugData <- as.data.frame(readxl::read_excel(system.file("extdata","exampleData.xlsx",
#'                                                      package = "melbstreambiota", mustWork = TRUE),
#'                                         sheet = 3))
#' obsTable2 <- collateObsTable(bugData, sampprTable)
#' #Check that data imported in both forms are equql
#' sum(obsTable1[-1] - obsTable2[-1])  # = 0
#' @export
collateObsTable <- function(bugData, sampprTable) {
  sampprTable$samppr <- as.vector(sampprTable$samppr)
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

  if (!(dim(bugData)[1] == length(sampprTable$samppr)) & !is.numeric(bugData[1,2]))
  {
    if (sum(names(bugData) %in% c("samppr","bugcode")) == 2)
    {
      bugData$samppr <- as.vector(bugData$samppr)
      # bugData$samppr[which(!is.na(as.numeric(substr(bugData$samppr,1,1))))] <-
      #   paste("X", bugData$samppr[which(!is.na(as.numeric(substr(bugData$samppr,1,1))))], sep = "")
      bugData <- as.data.frame.matrix(table(bugData[c("samppr","bugcode")]))
      bugData <- data.frame(samppr = row.names(bugData), bugData)
      row.names(bugData) <- 1:length(bugData$samppr)
      if (sum(is.na(match(bugData$samppr, sampprTable$samppr))) != 0)
        stop("samppr values in sample-pair table do not match values in bug data table", call. = FALSE)
    }else{
      stop("The bug data does not seem to be in the required format.", call. = FALSE)
    }
  }
  if (sum(is.na(match(bugData$samppr,sampprTable$samppr))) > 0)
  {
    stop("samppr values in sample-pair table do not match values in bug data table", call. = FALSE)
    #     }
  }
  bugsList <- untab(bugData, rows = "samppr", cols = "bugcode")
  bugsList <- bugsList[order(bugsList$samppr), c("samppr","bugcode","count")]
  bugsList <- unique(bugsList[,1:2])
  #shorten any long bugcodes
  bugsList$bugcode <- substr(bugsList$bugcode,1,4)
  #Combine chironiminae tribes and use old Odonata taxonomy
  w <- vector()
  if (sum(bugsList$bugcode %in% c("QDAG","QDAH","QDAI")) > 0)
  {
    bugsList[bugsList$bugcode %in% c("QDAG","QDAH","QDAI"),]$bugcode <- "QDAJ"
    w <- c(w, "Chironominae tribes combined. ")
  }
  if (sum(bugsList$bugcode %in% c("QO23", "QO26", "QO28", "QO27", "QO30")) > 0)
  {
    bugsList$bugcode[bugsList$bugcode == "QO23" | bugsList$bugcode == "QO26" |
                       bugsList$bugcode == "QO28" | bugsList$bugcode == "QO27" |
                       bugsList$bugcode == "QO30"] <- "QO16"
    w <- c(w, "'Old' odonate taxonomy used: new families split from Corduliidae have been consolidated. ")
  }
  exclFams <- as.vector(unique(bugsList$bugcode)[!unique(bugsList$bugcode) %in% melbstreambiota::taxon.classes$fam])
  if (length(exclFams) > 0)
    w <- c(w, "The following bugcodes were excluded as they are not used in LUMaR calculations:\n",
           paste(exclFams, collapse = ", "), ".")
  if(sum(unique(bugsList$bugcode) %in% melbstreambiota::bugfams$bugcode[melbstreambiota::bugfams$modelled == 1]) < 59){
    w <- c(w, cat("Your bugData does not include any entries for the following families that are used by LUMaR.
This may, of course, be correct, but check for any unexpected omissions, and check bugcode/taxonomy. \n",
                    paste(melbstreambiota::bugfams$bugcode[bugfams$modelled == 1 &
                                              !melbstreambiota::bugfams$bugcode %in% unique(bugsList$bugcode)],"\n")))
  }
  if (length(w) > 0)
  warning(w, call. = FALSE)
  bugsList <- bugsList[!is.na(match(bugsList$bugcode, melbstreambiota::taxon.classes$fam)),]

  obs.table1 <- as.data.frame.matrix(table(bugsList[c("samppr","bugcode")]))
  obs.table1 <- data.frame(samppr = row.names(obs.table1), obs.table1)
  row.names(obs.table1) <- 1:length(obs.table1$samppr)
  obs.table <- data.frame(samppr = as.vector(obs.table1$samppr),
                          matrix(0, nrow = length(obs.table1$samppr), ncol = 59))
  names(obs.table)[-1] <- melbstreambiota::taxon.classes$fam
  obs.table[match(names(obs.table1),names(obs.table))] <- obs.table1
  obs.table <- obs.table[match(sampprTable$samppr, obs.table$samppr),]
  obs.table
}

#' Convert a data frame of probabilities of occurrence of 59 families to presence-absence
#'
#' @param prob59 a data.frame with first column samppr (or row.names = samppr), and 59 columns with names = \code{taxon.classes$fam}.
#' Data for each samppr are 1s for observed presences and 0 for absences
#' @return  A data.frame the same structure as prob59, but with probabilities of occurrence
#' converted to presences (1) or absences (0)
#' @details Presence is inferred if the probability of occurrence (rounded to 3 decimal places) is
#' greater than or equal to the \code{pa.threshold} for each family as listed in \code{taxon.classes}
#' @examples
#' test59preds <- predCurrNHI59(melbstreambiota::mwstreams[1:4,])
#' prob59ToPA(test59preds$predCurr)
#' @export
prob59ToPA <- function(prob59) {
  pa59 <- prob59
    sampprsColumn <- ifelse(names(prob59)[1] %in% c("samppr", "subc"), 1, 0)
    if (sampprsColumn == 1) {
    pa59[-1] <- 0
    }else{
      pa59 <- 0
    }
for (i in 1:59) {
  pa59[round(prob59[,i + sampprsColumn],3) >= melbstreambiota::taxon.classes$pa.threshold[i],
       i + sampprsColumn] <- 1
  }
  pa59
}

#' Update the SRI_48mth_weighted data used by the package
#'
#' @return  A message reporting the most recent date for which SRI_48mth_weighted data is available.
#' @details Running this model downloads the latest version of sri48moW.rda from
#' https://osf.io/mcxrq/ and saves it to cache for use by the package
#' @examples
#' update_sri()
#' @export
update_sri <- function(){
  max_date <- max(sri48moW$date)
  dl_files <- osfr::osf_ls_files(osfr::osf_retrieve_node("mcxrq"))
  dl_files <- dl_files[dl_files$name == "sri48moW.rda",]
  x <- invisible(hoardr::hoard()$cache_path_set("melbstreambiota", type = 'user_data_dir'))
  dl_files <- osfr::osf_ls_files(osfr::osf_retrieve_node("mcxrq"))
  dl_files <- dl_files[dl_files$name == "sri48moW.rda",]
  invisible(osfr::osf_download(dl_files, path = x,
                             conflicts = "overwrite"))
  load(paste0(x,"/sri48moW.rda"))
  max_date_new <- max(sri48moW$date)
  if(max_date == max_date_new){
    cat(paste0("Latest SRI_48mth_weighted value is for ", max_date_new,".
               Your package data was up to date and was not updated. \n"))
  }else{
    cat(paste0("Latest SRI_48mth_weighted value is for ", max_date_new,".
               Your package data has been updated. Restart R or reload the package to use the updated data.\n"))
  }
}

