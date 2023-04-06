#' Plot a map of Melbourne's streams colour-coded by a variable
#'
#' @param map_var A vector (length 8246) of variable values for each subc in mwstreams (the function assumes that the vector is in the same order as mwstreams)
#' @param nbreaks number of classes to be plotted: equals n argument in classInt
#' @param style chosen style as in \code{classIntervals}: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks
#' @param varName character string for legend title
#' @param palette an RcolorBrewer palette (see \code{RColorBrewer::brewer.pal})
#' @param rev inverts palette if TRUE
#' @param legend plots legend if TRUE
#' @param legend.cex character expansion factor for legend
#' @param fixedBreaks NA unless style = "fixed", and then must equal a vector of breaks with length nbreaks + 1
#' @param subcSubset if NA the whole stream network is mapped.
#'  If a vector of subcs (a subset of mwstreams$subc) is provided, only that subset will be mapped.
#' @param ... other arguments to be passed to \code{classInt::classIntervals} or \code{sp::plot}
#' @return A plot of Melbourne's streams.
#' @details The "fixed" style permits a \code{classIntervals} object to be specified with given breaks,
#' set in the fixedBreaks argument; the length of fixedBreaks should be nbreaks+1; this style can be
#' used to insert rounded break values.
#' The "jenks" style is not implemented.
#' @seealso \code{\link[classInt]{classIntervals}}, \code{\link[RColorBrewer]{brewer.pal}}
#' @examples
#'  plotMWstreamsByVar(melbstreambiota::mwstreams$AttForest_L35W1000, style = "fixed",
#'                    fixedBreaks = seq(0,1,0.2), varName = "Attenuated Forest Cover",
#'                    legend.cex = 0.75)
#' @export
plotMWstreamsByVar <- function(map_var,
                               nbreaks = 5,
                               style = "quantile",
                               varName = "Variable",
                               palette = "Spectral",
                               rev = FALSE,
                               legend = TRUE,
                               legend.cex = 1.5,
                               fixedBreaks = NA,
                               subcSubset = NA, ...) {
  graphics::par(mar = c(0,0,0,0))
  if (is.na(subcSubset[1])){
    mapToDraw <- melbstreambiota::mwstreams_map
  }else{
    mapToDraw <- melbstreambiota::mwstreams_map[melbstreambiota::mwstreams_map$subc %in% subcSubset,] #   #
  }
  if (nbreaks > 12) stop("nbreaks must be < 12 for the Spectral palette", call. = FALSE)
  if (style == "jenks")  stop("the 'jenks' style is not implemented", call. = FALSE)
  if (length(unique(map_var[!is.na(map_var)])) > 1) {
    map_var <- map_var[match(mapToDraw$subc, melbstreambiota::mwstreams$subc)]
    varCl <- classInt::classIntervals(map_var, n = nbreaks, style = style,
                                      fixedBreaks = fixedBreaks, ...)
    varCol <- RColorBrewer::brewer.pal(n = nbreaks,
                                       name = palette)[match(classInt::findCols(varCl),1:nbreaks)]
    if (rev)
      varCol <- rev(RColorBrewer::brewer.pal(n = nbreaks,
                                             name = palette))[match(classInt::findCols(varCl),1:nbreaks)]
    plot(mapToDraw$geom, lwd = 1.5, col = varCol, ...)
  }else{
    ##This assumes that a single value will be the maximum - not always true, so not ideal, but often true for lumar
    varCl <- classInt::classIntervals(fixedBreaks, n = nbreaks, style = style,
                                      fixedBreaks = fixedBreaks, ...)
    plot(mapToDraw$geom, lwd = 1.5,
         col = RColorBrewer::brewer.pal(n = nbreaks,
                                        name = palette)[max(which(varCl$brks <= unique(map_var[!is.na(map_var)])))  - 1], ...)
  }
  plot(melbstreambiota::mwcoast_map$geom, add = TRUE)
  if (legend) {
    #the following allows for some classIntervals styles (e.g. sd) to override the n argument
    nbreaks <- length(varCl$brks) - 1
    legPal <- rev(RColorBrewer::brewer.pal(n = nbreaks, name = palette))
  if (rev) legPal <- RColorBrewer::brewer.pal(n = nbreaks, name = palette)
  graphics::legend("topright",
         legend = signif(rev(varCl$brks[-1]),2),
         pch = 22,col = NA, cex = legend.cex,
         pt.bg = legPal,
         pt.cex = legend.cex*3, title = varName, box.col = "white")
  }
}

#' Plot a map of Melbourne's streams (or a subset) by a single colour
#'
#' @param line_col a single colour
#' @param subcSubset if NA the whole stream network is mapped.
#'  If a vector of subcs (a subset of mwstreams$subc) is provided, only that subset will be mapped.
#' @param ... other arguments to be passed to \code{classInt::classIntervals} or \code{sp::plot}
#' @return A plot of the streams of Melbourne
#' @details A more flexible option for \code{plotMWstreamsByVar}, when var is a single value
#' @export
plotMWstreams1col <- function(line_col,
                              subcSubset = NA, ...) {
  graphics::par(mar = c(0,0,0,0))
  if (is.na(subcSubset[1])) {
    mapToDraw <- melbstreambiota::mwstreams_map
  }else{
    mapToDraw <- melbstreambiota::mwstreams_map[melbstreambiota::mwstreams_map$subc %in% subcSubset,]
  }
     plot(mapToDraw$geom, lwd = 1.5,
             col = line_col, ...)
    plot(melbstreambiota::mwcoast_map$geom, add = TRUE)
}
