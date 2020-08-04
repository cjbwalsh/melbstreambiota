# #These data were compiled in the file
# #""~/wergdrobo/Staff_Folders/ChrisW/newBugModelsMay2017/compileDataforMelbstreambiotaPackage.R"
#'
# #' Boosted Regression Tree models of 59 macroinvertebrate families.
# #'
# #' A list containing 59 boosted-regression tree models (class \code{gbm}).
# #' Each list element is a model, named by the macroinvertebrate code of
# #' the family as listed in \code{taxon.classes} and \code{bugfams}
# #'
# #' @format a list of 59 \code{gbm} objects
# #'
# #' @source Walsh, C. J. (in preparation) LUMaR: a sensitive macroinvertebrate
# #' index of stream condition combining observed:expected ratios and sensitivity
# #' weightings.
#' "bestModelsBugfams"

#' SIGNAL and other index grades for 174 macroinvertebrate families
#'
#' A data frame listing macroinvertebrate families and their grades for various SIGNAL variants and Rheophily index
#'
#' @format a data frame of 174 rows and 9 columns
#' \describe{
#'   \item{fam}{bugcode, consistent with EPA Victoria and AUSRIVAS conventions}
#'   \item{family}{family name}
#'   \item{SIGNAL2}{SIGNAL grade from Chessman 2003}
#'   \item{SIGNALWoV2001}{}
#'   \item{SIGNALWoV2003}{}
#'   \item{SIGNALWesPt2003}{}
#'   \item{SIGNALYarra2003}{}
#'   \item{Rheophily index}{from Bond et al 2012}
#'   \item{Comment}{}
#'   \item{note}{}
#' }
"bugfams"

#' Fish species and platypus groups modelled
#'
#' A data frame listing 23 fish species and associated codes (and two platypus groups) used in models
#'
#' @format a data frame of 25 rows and 4 columns
#' \describe{
#'   \item{vertcode}{the code used to identify models in the package}
#'   \item{species}{}
#'   \item{authority}{}
#'   \item{Commmon name}{}
#' }
"vertSpp"

#' A list of subcatchments each with a list of all upstream subcatchments that contain stream reaches in mwstreams
#'
#'
#' @format a list of length 8257
"streamsubcs.allus2017"

#' Details of the 59 modelled families
#'
#' A data frame listing the 59 families that
#' are modelled in \code{bestModelsBugfams}, with relevant statistics
#'
#' @format a data frame of 59 rows and 10 columns
#' \describe{
#'   \item{fam}{bugcode, consistent with EPA Victoria and AUSRIVAS conventions}
#'   \item{family}{family name}
#'   \item{devexp}{proportion explained deviance explained by the model}
#'   \item{pa.threshold}{presence-absence threshold used in the LUMaR index}
#'   \item{sens.gp}{sensitivity group used in the LUMaR index}
#'   \item{SIGNAL}{SIGNAL grade (Chessman 1995)}
#'   \item{SIGNAL2}{SIGNAL2 grade (Chessman 2003)}
#'   \item{unexp.grade}{sensitivity grade used in the LUMaR index}
#'   \item{sens.grade}{unexpected grade used in the LUMaR index}
#' }
#' @source Walsh, C. J. (in preparation) LUMaR: a sensitive macroinvertebrate
#' index of stream condition combining observed:expected ratios and sensitivity
#' weightings.
"taxon.classes"

#' Environmental predictor variables for 8246 stream reaches (subcs) across the Melbourne region
#'
#' A data frame data listing environmental predictor variables for all \code{subc}
#' values in \code{mwStreamsMap}.
#'
#' @format a data frame of 8246 rows and 61 columns
#' \describe{
#'   \item{subc}{unique subcatchment ID corresponding to subcs in \code{mwStreamsMap}}
#'   \item{segmentno}{geofabric segmentno (BoM, 2011)}
#'   \item{AttImp_L9}{Attenuated imperviousness in 2006 as calculated by Walsh and Webb 2014
#'   (exponential weighting to nearest stormwater drain or stream with a half-decay
#'   distance of 9.4 m).  Variously also called dai9 (as it is in the subcs table),
#'   AI (in Walsh and Kunapo, 2009; Walsh and Webb 2013, 2014, 2016), and DCI (in
#'   various MW publications)}
#'   \item{CatchmentArea_km2_InclDams}{catchment area calculated from the DCI
#'   subcatchment layer, and for sites downstream of large dams that rarely spill,
#'   includes the catchment upstream of the dam. (note that these areas were
#'   excluded in the determination of AttImp_L9)}
#'   \item{Elevation_m}{Elevation above sea-level in m (derived from 10 m DEM).
#'   [used in early macroinvertebrate models of Walsh and Webb (2014, 2016),
#'   but not subsequently]}
#'   \item{meanAnnQ_mm}{An estimate of mean annual runoff depth in mm,
#'   derived from the gridded monthly runoff data from The Australian Water
#'   Availability Project (AWAP; Raupach et al. 2009)}
#'   \item{SolarRadiation}{STRANRAD from geofabric (BoM, 2011)}
#'   \item{mnAnnAirTm_deg}{mean annual air temperature in Degrees Celsius
#'   at the centroid of each subc (derived from 5-km grid from BOM).}
#'   \item{AFb10L1000}{Attenuated forest cover, calculated as the
#'   unweighted (laterally) forest cover less than or equal to 10m
#'   from the stream, and exponentially weighted upstream with a
#'   half-decay distance of 1000 m. [used in the temperature model]}
#'   \item{NHImnAnnStreamTm}{Predicted stream temperature in the absence of
#'   human impacts (full forest cover, no attenuated imperviousness), as
#'   predicted by the mean annual stream temperature model
#'   [originally planned to be used in the biological models, but as it is
#'   a function of mnAnnAirTm_deg, meanAnnQ_mm and CatchmentArea_km2_InclDams,
#'   mnAnnAirTm_deg is used instead]}
#'   \item{AttForest_L35W1000}{Attenuated forest cover with HDD overland to
#'   nearest stream of 35 m, and upstream 1 km (Walsh and Webb, 2014)}
#'   \item{CatIgneous}{Percentage of catchment with igneous geology:
#'   CATIGN from Geofabric (BoM, 2011)}
#'   \item{CVMonthlyQ}{RUNMTHCOFV (Monthly coefficient of variation in
#'   discharge from Geofabric (BoM, 2011)}
#'   \item{AttImpMin4k_L9}{Minimum AttImp_L9 within 4 km downstream of a
#'   reach (Martin et al 2014)}
#'   \item{LWDBank}{Number of large woody debris pieces per area of bank,
#'   calculated only for large streams surveyed for the Index of Stream
#'   Condition}
#'   \item{vegBank}{Proportion of bank along reach covered by vegetation,
#'   calculated only for large streams surveyed for the Index of Stream
#'   Condition}
#'   \item{vegBed}{Proportion of bed along reach covered by vegetation,
#'   calculated only for large streams surveyed for the Index of Stream
#'   Condition}
#'   \item{Att.Forest_10mBuffer}{AttForest_L35W1000, but assuming a
#'   10-m buffer along all streams of the region}
#'   \item{Att.Forest_20mBuffer}{AttForest_L35W1000, but assuming a
#'   20-m buffer along all streams of the region}
#'   \item{Att.Forest_40mBuffer}{AttForest_L35W1000, but assuming a
#'   40-m buffer along all streams of the region}
#'   \item{Att.Forest_100mBuffer}{AttForest_L35W1000, but assuming a
#'   100-m buffer along all streams of the region}
#'   \item{strLen_m}{total length of stream in each subc}
#'   \item{strLenNat_m}{total length of natural stream in each subc}
#'   \item{strLenCha_m}{total length of channel in each subc}
#'   \item{strLenPip_m}{total length of piped stream in each subc}
#'   \item{nPartBarriersDS_pre1997}{Number of partial barriers downstream before 1997}
#'   \item{nPartBarriersDS_1997}{Number of partial barriers downstream in 1997}
#'   \item{nPartBarriersDS_1999}{Number of partial barriers downstream in 1999}
#'   \item{nPartBarriersDS_2000}{Number of partial barriers downstream in 2000}
#'   \item{nPartBarriersDS_2002}{Number of partial barriers downstream in 2002}
#'   \item{nPartBarriersDS_2004}{Number of partial barriers downstream in 2004}
#'   \item{nPartBarriersDS_2005}{Number of partial barriers downstream in 2005}
#'   \item{nPartBarriersDS_2006}{Number of partial barriers downstream in 2006}
#'   \item{nPartBarriersDS_2007}{Number of partial barriers downstream in 2007}
#'   \item{nPartBarriersDS_2008}{Number of partial barriers downstream in 2008}
#'   \item{nPartBarriersDS_2009}{Number of partial barriers downstream in 2009}
#'   \item{nPartBarriersDS_2010}{Number of partial barriers downstream in 2010}
#'   \item{nPartBarriersDS_2016}{Number of partial barriers downstream in 2016}
#'   \item{nFullBarriersDS_pre2007}{Number of full barriers downstream before 2007}
#'   \item{nFullBarriersDS_2007}{Number of full barriers downstream in 2007}
#'   \item{nFullBarriersDS_2008}{Number of full barriers downstream in 2008}
#'   \item{nFullBarriersDS_2009}{Number of full barriers downstream in 2009}
#'   \item{nFullBarriersDS_2012}{Number of full barriers downstream in 2012}
#'   \item{nFullBarriersDS_2014}{Number of full barriers downstream in 2014}
#'   \item{MU_ID}{Melbourne Water Management Unit}
#'   \item{SYS_ID}{Melbourne Water System (broadly catchments)}
#'   \item{AttImp_L9_2014}{AttImp_L9 updated to 2014 accounting for new development}
#'   \item{AttImp_L9_Ultimate}{AttImp_L9 updated to a likely ~2050 future assuming
#'   infill development of areas zoned for urban development in the urban growth boundary}
#'   \item{AttImpMin4k_L9_2014}{AttImpMin4k_L9 updated to 2014 accounting for new development}
#'   \item{AttImpMin4k_L9_Ultimate}{AttImpMin4k_L9 updated to a likely ~2050 future assuming
#'   infill development of areas zoned for urban development in the urban growth boundary}
#'   \item{AttForest_L35W1000_1996}{AttForest_L35W1000 calculated from 1996 imagery:
#'   MW data in progress: as of this version this field has only been calculated for
#'   "MARI" subcs: all other values in this field = AttForest_L35W1000 (2006)}
#'   \item{AttForest_L35W1000_2016}{AttForest_L35W1000 calculated from 2016 imagery:
#'   MW data in progress: as of this version this field has only been calculated for
#'   "MARI" subcs: all other values in this field = AttForest_L35W1000 (2006)}
#'   \item{nspring}{Number of Spring samples: specific to samples,
#'   but set to 0 (i.e. 2 autumn samples) for running scenarios}
#'   \item{nriff}{Number of riffle samples: specific to samples,
#'   but set to 1 (i.e. a riffle-edge pair) for running scenarios }
#'   \item{processN}{Process: 0 = lab-sort, 1 = field-sort, set to
#'   for running scenarios}
#'   \item{SRI_48mth_weighted}{specific to samples but set to 1 (i.e. the 48-month linearly-weighted antecedent discharge
#'   was equivalent to the long-term mean discharge: 0.5 = half the long-term mean, 2 = twice the long-term mean)}
#'   }
#' @source {BoM (2011) Australian hydrological geospatial fabric (geofabric) product guide. Version 2.0 – November 2011. Australian Government, Bureau of Meteorology, Canberra.\cr
#'  Martin, E.H., Walsh, C.J., Serena, M. & Webb, J.A. (2014) Urban stormwater runoff limits distribution of platypus. Austral Ecology, 39, 337–345.\cr
#'  Raupach, M.R., Briggs, P.R., Haverd, V., King, E.A., Paget, M. & Trudinger, C.M. (2009) Australian Water Availability Project (AWAP): CSIRO Marine and Atmospheric Research Component: Final Report for Phase 3. The Centre for Australian Weather and Climate Research, Canberra, Australia.\cr
#'  Walsh, C. J. (in preparation) LUMaR: a sensitive macroinvertebrate index of stream condition combining observed:expected ratios and sensitivity weightings.\cr
#'  Walsh, C.J. & Kunapo, J. (2009) The importance of upland flow paths in determining urban effects on stream ecosystems Journal of the North American Benthological Society, 28, 977–990.\cr
#'  Walsh, C.J. & Webb, J.A. (2013) Predicting stream macroinvertebrate assemblage composition as a function of land use, physiography and climate: a guide for strategic planning for river and water management in the Melbourne Water region. Department of Resource Management and Geography, The University of Melbourne, Melbourne.\cr
#'  Walsh, C.J. & Webb, J.A. (2014) Spatial weighting of land use and temporal weighting of antecedent discharge improves prediction of stream condition. Landscape Ecology, 29, 1171–1185.\cr
#'  Walsh, C.J. & Webb, J.A. (2016) Interactive effects of urban stormwater drainage, land clearance, and flow regime on stream macroinvertebrate assemblages across a large metropolitan region. Freshwater Science, 35, 324–339.}
"mwstreams"

#' Map of streams of the Melbourne region
#'
#' A SpatialLinesDataDataFrame of all modelled reaches in the region
#'
#' @format A SpatialLinesDataDataFrame of 25945 rows (polylines) and 4 columns
#' \describe{
#'   \item{subc}{reach ID as in \code{mwstreams}}
#'   \item{streamName}{Name of stream}
#'   \item{source}{Original source of the mapped stream line}
#'   \item{length_m}{length of each polyline in m (note there may be
#'   multiple polylines per subc and they are summed in \code{mwstreams$strLen_m)}}
#' }
#' @source Melbourne Water: a combination of their Natural Waterway and channel layers.
#' This stream network is an augmented and corrected version of the DCI stream layer used
#' by Walsh and Webb 2013.
"mwStreamsMap"

#' Map of the Melbourne region coastline
#'
#' A SpatialLinesDataDataFrame of the coastline
#'
#' @format A SpatialLinesDataDataFrame of 37 rows (polylines) and 1 columns
#' \describe{
#'   \item{ID}{unique ID}
#' }
#' @source \url{http://land.vic.gov.au}
"mwCoastMap"

#' Monthly 48-month linearly-weighted antecedent discharge since 1986 for all mwstream subcs
#'
#' A data frame data of subcs each with monthly 48-month linearly-weighted
#' antecedent discharge since (1986-2014 [some to 2016]) for all mwstream subcs,
#' calculated as described by Walsh and Webb (2014, 2016)
#'
#'
#' @format a data frame of 2806548 rows and 3 columns
#' \describe{
#'   \item{date}{\code{POSIXct} date, time zone "UTC"}
#'   \item{subc}{subcatchment ID equivalent to \code{mwstreams$subc}}
#'   \item{SRI_48mth_weighted}{48-month linearly-weighted antecedent discharge as a proportion of mean annual discharge}
#'   }
#'   @details The general method for \code{SRI_48mth_weighted} was described by Walsh and Webb 2014.
#'   These data are the variant, calculated from the Australian Water Availability Project data
#'   (AWAP; Raupach et al. 2009) as described in Walsh (in preparation).  The \code{date} field equals
#'   the first of each month from Jan 1986 to Feb 2014 for most subcs, and to July 2016 for some (Main Creek,
#'   French Island, Bass, Lang Lang, Yallock, and Bunyip catchments).
#' @source Walsh, C. J. (in preparation) LUMaR: a sensitive macroinvertebrate
#' index of stream condition combining observed:expected ratios and sensitivity
#' weightings.
"sri48moW"

##Added to CIs.rda, which includes CIs for Lumar, SIGNAL, SIGNAL2 and NsensFams
##LUMaR confidence intervals
##'
##A data frame data of LUMaR median, and 90% confidence intervals across its range of values
##'
##@format a data frame of 900 rows and 4 columns
##\describe{
##  \item{lumar}{Observed range of LUMaR scores}
##  \item{lwr10}{lower 90-percent confidence interval of predicted LUMaR score in 221 test sites}
##  \item{median}{median predicted LUMaR score in 221 test sites}
##  \item{upr10}{upper 90-percent confidence interval of predicted LUMaR score in 221 test sites}
##  }
##  @details See Walsh (in preparation) for derivation of these data, that are used to provide
##  indicative confidence intervals around LUMaR predictions in diagnostic functions.
##@source Walsh, C. J. (in preparation) LUMaR: a sensitive macroinvertebrate
##index of stream condition combining observed:expected ratios and sensitivity
##weightings.
#"lumarCIs"
