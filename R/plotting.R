### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("groups", "thin", "whGrobNamesi",
                           "xmax", "xmin", "ymax", "ymin"))
}

################################################################################
#' \code{Plot}: Fast, optimally arranged, multipanel plotting
#'
#' This can take objects of type \code{Raster*}, \code{SpatialPoints*},
#' \code{SpatialPolygons*}, and any combination of those.
#' These can be provided as individual objects, or a named list.
#' If a named list, the names either represent a different original object in the
#' calling environment and that will be used, or if the names don't exist in the
#' calling environment, then they will be copied to \code{.quickPlotEnv} for reuse later.
#' It can also handle \code{ggplot2} objects or \code{base::histogram} objects
#' created via call to \code{exHist <- hist(1:10, plot = FALSE)}. It can also take
#' arguments as if it were a call to \code{plot}. In this latter
#' case, the user should be explicit about naming the plot area using \code{addTo}.
#' Customization of the \code{ggplot2} elements can be done as a normal
#' \code{ggplot2} plot, then added with \code{Plot(ggplotObject)}.
#'
#' \strong{NOTE:} Plot uses the \pkg{grid} package; therefore, it is NOT compatible
#' with base R graphics. Also, because it does not by default wipe the plotting device
#' before plotting, a call to \code{\link{clearPlot}} is helpful to resolve
#' many errors. Careful use of the other device tools, such as \code{dev.off()} and
#' \code{dev.list()} might also clear problems that may arise.
#'
#' If \code{new = TRUE}, a new plot will be generated, but only in the figure region that
#' has the same name as the object being plotted.
#' This is different than calling \code{clearPlot(); Plot(Object)},
#' i.e,. directly before creating a new Plot. \code{clearPlot()} will clear the entire
#' plotting device.
#' When \code{new = FALSE}, any plot that already exists will be overplotted,
#' while plots that have not already been plotted will be added.
#' This function rearranges the plotting device to maximize the size of all the
#' plots, minimizing white space.
#' If using the RStudio IDE, it is recommended to make and use a new device
#' with \code{dev()}, because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Each panel in the multipanel plot must have a name.
#' This name is used to overplot, rearrange the plots, or overlay using
#' \code{addTo} when necessary.
#' If the \code{...} are named spatialObjects, then \code{Plot} will use
#' these names. However, this name will not persist when there is a future call
#' to \code{Plot} that forces a rearrangement of the plots.
#' A more stable way is to use the object names directly, and any layer names
#' (in the case of \code{RasterLayer} or \code{RasterStack} objects).
#' If plotting a RasterLayer and the layer name is "layer" or the same as the
#' object name, then, for simplicity, only the object name will be used.
#' In other words, only enough information is used to uniquely identify the plot.
#'
#' Because of modularity, Plot must have access to the original objects that were
#' plotted. These objects will be used if a subsequent Plot event forces a
#' rearrangement of the Plot device. Rather than saving all the plot information
#' (including the data) at each Plot
#' call (this is generally too much data to constantly make copies),
#' the function saves a pointer to the original R object. If the plot needs
#' to be rearranged because of a future addition, then Plot will search for that
#' original object that created the first plots, and replot them. This has several
#' consequences. First, that object must still exist and in the same environment.
#' Second, if that object has changed between the first time it is plot and any
#' subsequent time it is replotted (via a forced rearrangement), then it will take
#' the object *as it exists*, not as it existed. Third, if passing a named list
#' of objects, Plot will either create a link to objects with those names in the
#' calling environment (e.g., .GlobalEnv) or, if they do not exist, then Plot
#' will make a copy in the hidden .quickPlotEnv for later reuse.
#'
#'
#' \code{cols} is a vector of colours that can be understood directly, or by
#' \code{colorRampePalette}, such as \code{c("orange", "blue")}, will give a
#' colour range from orange to blue, interploated.
#' If a list, it will be used, in order, for each item to be plotted.
#' It will be recycled if it is shorter than the objects to be plotted.
#' Note that when this approach to setting colours is used, any overplotting
#' will revert to the \code{colortable} slot of the object, or the default
#' for rasters, which is \code{terrain.color()}
#'
#' \code{cols} can also accept \code{RColorBrewer} colors by keyword if it is
#' character vector of length 1. i.e., this cannot be used to set many objects by keyword in
#' the same Plot call. Default \code{terrain.color()}. See Details.
#'
#' Some coloring will be automatic. If the object being plotted is a Raster, then
#' this will take the colorTable slot (can be changed via setColors() or other ways).
#' If this is a SpatialPointsDataFrame, this function will use a column called \code{colors}
#' and apply these to the symbols.
#'
#' Silently, one hidden object is made, \code{.quickPlot} in the
#' \code{.quickPlotEnv} environment, which is used for arranging plots in the
#' device window, and identifying the objects to be replotted if rearranging
#' is required, subsequent to a \code{new = FALSE} additional plot.
#'
#' This function is optimized to allow modular Plotting. This means that several
#' behaviours will appear unusual.
#' For instance, if a first call to \code{Plot} is made, the legend will reflect
#' the current color scheme. If a second or subsequent call to \code{Plot} is
#' made with the same object but with different colours (e.g., with \code{cols}),
#' the legend will not update. This behaviour is made with the decision that the
#' original layer takes precedence and all subsequent plots to that same frame
#' are overplots only.
#'
#' \code{speedup} is not a precise number because it is faster to plot an
#' non-resampled raster if the new resampling is close to the original number of
#' pixels.
#' At the moment, for rasters, this is set to 1/3 of the original pixels.
#' In other words, \code{speedup} will not do anything if the factor for
#' speeding up is not high enough (i.e., >3). If no sub-sampling is desired,
#' use a speedup value less than 0.1.
#'
#' These \code{gp*} parameters will specify plot parameters that are available
#' with \code{gpar()}. \code{gp} will adjust plot parameters, \code{gpText}
#' will adjust title and legend text, \code{gpAxis} will adjust the axes.
#' \code{size} adjusts point size in a \code{SpatialPoints} object.
#' These will persist with the original \code{Plot} call for each individual object.
#' Multiple entries can be used, but they must be named list elements and they
#' must match the \code{...} items to plot.
#' This is true for a \code{RasterStack} also, i.e., the list of named elements
#' must be the same length as the number of layers being plotted.
#' The naming convention used is: \code{RasterStackName$layerName}, i.e,
#' \code{landscape$DEM}.
#'
#' @param ... A combination of \code{spatialObjects} or some non-spatial objects.
#'            See details.
#'
#' @param new Logical. If \code{TRUE}, then the previous named plot area is wiped
#'            and a new one made; if \code{FALSE}, then the \code{...} plots will be
#'            added to the current device, adding or rearranging the plot layout
#'            as necessary. Default is \code{FALSE}. This currently works best if
#'            there is only one object being plotted in a given Plot call. However,
#'            it is possible to pass a list of logicals to this, matching the
#'            length of the ... objects. Use \code{clearPlot} to clear the whole
#'            plotting device.
#'
#' @param addTo Character vector, with same length as \code{...}.
#'              This is for overplotting, when the overplot is not to occur on
#'              the plot with the same name, such as plotting a
#'              \code{SpatialPoints*} object on a \code{RasterLayer}.
#'
#' @param gp A \code{gpar} object, created by \code{\link{gpar}} function,
#'           to change plotting parameters (see \pkg{grid} package).
#'
#' @param gpText A \code{gpar} object for the title text.
#'               Default \code{gpar(col = "black")}.
#'
#' @param gpAxis A \code{gpar} object for the axes.
#'               Default \code{gpar(col = "black")}.
#'
#' @param axes Logical or \code{"L"}, representing the left and bottom axes,
#'             over all plots.
#'
#' @param speedup Numeric. The factor by which the number of pixels is divided
#'                by to plot rasters. See Details.
#'
#' @param size Numeric. The size, in points, for \code{SpatialPoints} symbols,
#'             if using a scalable symbol.
#'
#' @param cols (also \code{col}) Character vector or list of character vectors of colours.
#'             See details.
#'
#' @param col (also \code{cols}) Alternative to \code{cols} to be consistent with \code{plot}.
#'            \code{cols} takes precedence, if both are provided.
#'
#' @param zoomExtent An \code{Extent} object. Supplying a single extent that is
#'                   smaller than the rasters will call a crop statement before
#'                   plotting. Defaults to \code{NULL}.
#'                   This occurs after any downsampling of rasters, so it may
#'                   produce very pixelated maps.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether a legend should be drawn.
#'               Default is \code{TRUE}.
#'
#' @param legendRange Numeric vector giving values that, representing the lower
#'                    and upper bounds of a legend (i.e., \code{1:10} or
#'                    \code{c(1,10)} will give same result) that will override
#'                    the data bounds contained within the \code{grobToPlot}.
#'
#' @param legendText Character vector of legend value labels.
#'                   Defaults to \code{NULL}, which results in a pretty numeric
#'                   representation.
#'                   If \code{Raster*} has a Raster Attribute Table (\code{rat};
#'                   see \pkg{raster} package), this will be used by default.
#'                   Currently, only a single vector is accepted.
#'                   The length of this must match the length of the legend, so
#'                   this is mostly useful for discrete-valued rasters.
#'
#' @param na.color Character string indicating the color for \code{NA} values.
#'                 Default transparent.
#'
#' @param zero.color Character string indicating the color for zero values,
#'                   when zero is the minimum value, otherwise, zero is
#'                   treated as any other color. Default transparent.
#'
#' @param pch see \code{?par}.
#'
#' @param title Logical or character string. If logical, it
#'              indicates whether to print the object name as the title
#'              above the plot. If a character string, it will print this
#'              above the plot. NOTE: the object name is used with \code{addTo},
#'              not the title.
#'
#' @param length Numeric. Optional length, in inches, of the arrow head.
#'
#' @param arr A vector of length 2 indicating a desired arrangement of plot
#'            areas indicating number of rows, number of columns.
#'            Default NULL, meaning
#'            let Plot function do it automatically.
#'
#' @param plotFn An optional function name to do the plotting internally, e.g.,
#'               "barplot" to get a barplot() call. Default "plot".
#'
#' @return Invisibly returns the \code{.quickPlot} class object.
#' If this is assigned to an object, say \code{obj}, then this can be plotted
#' again with \code{Plot(obj)}.
#' This object is also stored in the locked \code{.quickPlotEnv}, so can simply be
#' replotted with \code{rePlot()} or on a new device with \code{rePlot(n)},
#' where \code{n} is the new device number.
#'
#' @seealso \code{\link{clearPlot}}, \code{\link{gpar}}, \code{\link{raster}},
#' \code{\link{par}}, \code{\link{SpatialPolygons}}, \code{\link{grid.polyline}},
#' \code{\link{ggplot}}, \code{\link{dev}}
#'
#' @author Eliot McIntire
#' @export
#' @importFrom gridBase gridFIG
#' @importFrom ggplot2 ggplot
#' @importFrom raster crop is.factor
#' @importFrom grid upViewport pushViewport
#' @importFrom grid grid.rect grid.xaxis grid.yaxis current.parent gpar
#' @importFrom grDevices dev.cur dev.size
#' @include environment.R
#' @include plotting-classes.R
#' @include plotting-colours.R
#' @include plotting-helpers.R
#' @include plotting-other.R
#'
#' @rdname Plot
#'
#' @example inst/examples/example_Plot.R
#'
setGeneric(
  "Plot",
  signature = "...",
  function(..., new = FALSE, addTo = NULL,
           gp = gpar(), gpText = gpar(), gpAxis = gpar(), axes = FALSE,
           speedup = 1, size = 5, cols = NULL, col = NULL, zoomExtent = NULL,
           visualSqueeze = NULL, legend = TRUE, legendRange = NULL,
           legendText = NULL, pch = 19, title = TRUE, na.color = "#FFFFFF00", # nolint
           zero.color = NULL, length = NULL, arr = NULL, plotFn = "plot") { # nolint
    standardGeneric("Plot")
})

#' @rdname Plot
#' @export
setMethod(
  "Plot",
  signature("ANY"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes, speedup,
                        size, cols, col, zoomExtent, visualSqueeze, legend,
                        legendRange, legendText, pch, title, na.color, # nolint
                        zero.color, length, arr, plotFn) { # nolint
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.

    news <- sapply(new, function(x) x)
    # this covers the case where R thinks that there is nothing, but
    #  there may in fact be something.
    if (sum(news) > 0) {
      if (length(ls(.quickPlotEnv)) +
           numLayers(.quickPlotEnv[[paste0("quickPlot", dev.cur())]]) - 1 <= sum(news) |
          length(ls(.quickPlotEnv)) == 0) {
        clearPlot(dev.cur())
    }}

    # Determine object names that were passed and layer names of each
    scalls <- sys.calls()

    # This testthat is a work around:
    # A test_that call can be very long, with many function calls, including Plot and do.call,
    # even if they don't have anything to do with each other
    isDoCall <- grepl("^do.call", scalls) & grepl("Plot", scalls) & !grepl("test_that", scalls)

    dots <- list(...)
    if (is.list(dots[[1]]) & !is(dots[[1]], ".quickPlottables") &
       !is(dots[[1]], "communities") & !is(dots[[1]], "igraph") & !is(dots[[1]], "histogram")) {
      dots <- unlist(dots, recursive = FALSE)
      isList <- TRUE
      if (is.null(names(dots)))
        stop("If providing a list of objects to Plot, it must be a named list.")
    } else {
      isList <- FALSE
    }

    # Determine where the objects are located
    #  We need to know exactly where they are, so that they can be replotted later, if needed
    if (any(isDoCall)) {

      whFrame <- which(isDoCall)
      plotFrame <- sys.frame(whFrame - 1)
      if (is.null(dots$env))
        dots$env <- plotFrame
        #stop("Currently, Plot can not be called within a do.call. ",
        #     "Try passing a named list of objects to Plot instead.")
      dotObjs <- get(as.character(match.call(do.call, call = sys.call(whFrame))$args),
                     envir = plotFrame)
      plotArgs <- mget(names(formals("Plot")[-1]), sys.frame(whFrame - 2)) # 2 up with do.call
    } else {
      whFrame <- grep(scalls, pattern = "^Plot")
      dotObjs <- dots

      if (isList) {
        # perhaps push objects into an environment, if they are only in the list
        env <- sys.frame(whFrame - 1)
        onlyInList <- !unlist(lapply(names(dots), exists, envir = env, inherits = FALSE))
        if (any(onlyInList)) {
          assign(paste0("Dev", dev.cur()), new.env(hash = FALSE, parent = .quickPlotEnv),
                 envir = .quickPlotEnv)
          dots$env <- list2env(dots, envir = get(paste0("Dev", dev.cur()), envir = .quickPlotEnv))

        } else {
          dots$env <- env
        }

      }
      plotFrame <- sys.frame(whFrame)
      plotArgs <- mget(names(formals("Plot")), plotFrame)[-1]
    }

    # if user uses col instead of cols
    if (is.null(cols)) {
      if (!is.null(col)) {
        cols <- col
        plotArgs$cols <- cols
      }
    } else {
      if (!is.null(col)) {
        message("cols and col both supplied. Using cols")
      }
    }

    if (!is.null(dots$env)) {
      objFrame <- dots$env
      dotObjs$env <- NULL
    } else {
      objFrame <- plotFrame
    }

    whichQuickPlottables <- sapply(dotObjs, function(x) {
      is(x, ".quickPlottables")
    })

    if (!(all(!whichQuickPlottables) | all(whichQuickPlottables)))
      stop("Can't mix base plots with .quickPlottables objects in one function call. ",
              "Please call Plot for base plots separately.")

    # Create plotObjs object, which is a cleaned up version of the objects passed into Plot
    if (all(!whichQuickPlottables)) {
      ## if not a .quickPlottables then it is a pass to plot or points
      if (!exists(paste0("basePlots_", dev.cur()), envir = .quickPlotEnv))
        .assignQuickPlot(paste0("basePlots_", dev.cur()),
                         new.env(hash = FALSE, parent = .quickPlotEnv))
      mc <- match.call(get(plotArgs$plotFn), call(plotArgs$plotFn, quote(...)))
      mcPlot <- match.call(Plot, call = sys.call(whFrame))
      plotArgs$userProvidedPlotFn <- ("plotFn" %in% names(mcPlot))

      basePlotDots <- list()
      for (i in names(mc)[-1]) basePlotDots[[i]] <- eval(mc[[i]])
      plotObjs <- list(list(basePlotDots))
      plotXYArgs <- substitute(list(...))
      xAndY <- c("x", "y") %in% names(basePlotDots)
      xAndYLab <- c("xlab", "ylab") %in% names(basePlotDots)
      xAndYSum <- sum(xAndY)
      if (!is.null(basePlotDots$xlab) | !is.null(basePlotDots$ylab)) {
        plotArgs$axisLabels <- list(c(basePlotDots$xlab, basePlotDots$ylab))
        names(plotArgs$axisLabels[[1]]) <- c("x", "y")[xAndYLab]
      } else {
        plotArgs$axisLabels <- list(unlist(lapply(plotXYArgs[1:xAndYSum + 1], deparse)))
      }

      if (is.null(addTo)) {
        addTo <- "basePlot1"
      }
      plotArgs$addTo <- addTo

      if (is.null(mcPlot$axes)) {
        plotArgs$axes <- "L"
      }

      if (is.null(plotArgs$title)) {
        plotArgs$title <- mc$main
      }

      if (is.null(mcPlot$col)) {
        if (!any(unlist(lapply(dotObjs, function(x) {
          any(unlist(lapply(c("histogram", "igraph", "communities"), function(y) is(x, y))))
        })))) #default for histogram is NULL
          plotArgs$col <- "black"
      }
      plotArgs$main <- ""
      plotObjs[[1]][[1]]$main <- plotArgs$main
      basePlotDots$main <- plotArgs$main

      if (addTo %in% ls(.getQuickPlot(paste0("basePlots_", dev.cur())))) {
        plotObjsName <- paste0(addTo, "_",
                               length(ls(.getQuickPlot(paste0("basePlots_", dev.cur())))) + 1)
      } else {
        plotObjsName <- addTo
      }
      names(plotObjs) <- plotObjsName
      assign(plotObjsName, basePlotDots, envir = .getQuickPlot(paste0("basePlots_", dev.cur())))
      objFrame <- .getQuickPlot(paste0("basePlots_", dev.cur()))
    } else {
      # non base plots
      canPlot <- if (!is.null(names(whichQuickPlottables))) {
        whichQuickPlottables[names(whichQuickPlottables) != "env"]
      } else {
        whichQuickPlottables
      }

      if (!all(canPlot)) {
        if ((sum(canPlot) - length(grep(pattern = "col", names(canPlot)))) > 0) { # nolint
          # don't message if col is passed
          message(paste(
            "Plot can only plot objects of class .quickPlottables.",
            "Use 'showClass(\".quickPlottables\")' to see current available",
            "classes"
          ))
        }
      }
      plotObjs <- dotObjs[whichQuickPlottables]
    }

    nonPlotArgs <- dotObjs[!whichQuickPlottables]
    if (any(grepl(pattern = "col", names(nonPlotArgs)))) {
      nonPlotArgs$col <- "black"
    }

    # intercept cases that don't make sense, and give meaningful error
    if (!is.null(addTo)) {
      if (!tryCatch(
        addTo %in%
          unlist(layerNames(get(paste0("quickPlot", dev.cur()), envir = .quickPlotEnv))),
        error = function(x) FALSE)) {
          plotArgs$addTo <- NULL
        }
    }

    # Create a .quickPlot object from the plotObjs and plotArgs
    isQuickPlot <- sapply(plotObjs, function(x) is(x, ".quickPlot"))
    isQuickPlotLong <- rep(isQuickPlot, unlist(lapply(plotObjs, numLayers)))

    newQuickPlots <- .makeQuickPlot(
      plotObjs, plotArgs, whichQuickPlottables, env = objFrame)

    if (exists(paste0("quickPlot", dev.cur()), envir = .quickPlotEnv)) {
      currQuickPlots <- .getQuickPlot(paste0("quickPlot", dev.cur()))

      visualSqueeze <- if (is.null(visualSqueeze)) {
        currQuickPlots$curr@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateQuickPlot(newQuickPlots, currQuickPlots)

      # Do all the plots fit into the device?
      newArr <- (
        length(updated$curr@quickPlotGrobList) >
          prod(currQuickPlots$curr@arr@columns, currQuickPlots$curr@arr@rows)
      ) | !identical(currQuickPlots$curr@arr@ds, dev.size())

      if (newArr) {
        updated$needPlotting <- lapply(updated$needPlotting, function(x) {
          sapply(x, function(y) TRUE)
        })
        updated$isReplot <- lapply(updated$isReplot, function(x) {
          sapply(x, function(y) TRUE)
        })
        clearPlot(removeData = FALSE)
      }
    } else if (all(isQuickPlot)) {
      currQuickPlots <- .makeQuickPlot()
      newQuickPlots <- plotObjs[[1]]

      visualSqueeze <- if (is.null(visualSqueeze)) {
        newQuickPlots@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateQuickPlot(newQuickPlots)
      newArr <- TRUE
    } else {
      currQuickPlots <- .makeQuickPlot()
      updated <- .updateQuickPlot(newQuickPlots)
      newArr <- TRUE
    }

    # Section 2 # Optimal Layout and viewport making
    # Create optimal layout, given the objects to be plotted, whether legend and axes are to be
    #  plotted, and visualSqueeze
    if (newArr) {
      if (is.null(visualSqueeze)) {
        visualSqueeze <- 0.75
      }
      updated$curr@arr <- .arrangeViewports(updated$curr, arr = arr)
      updated$curr@arr@layout <- .makeLayout(
        updated$curr@arr, sapply(visualSqueeze, max), sapply(legend, any),
        sapply(axes, function(x) !any(x == TRUE))
      )
    }

    # Create the viewports as per the optimal layout
    if (length(newQuickPlots@quickPlotGrobList) > 0) {
      vps <- .makeViewports(updated$curr, newArr = newArr)
      upViewport(0)
      pushViewport(vps$wholeVp, recording = FALSE)
      upViewport(0)
    }
    updated$curr@arr@extents <- vps$extents
    updated$curr@arr@names <- names(updated$curr@quickPlotGrobList)

    quickSubPlots <- updated$curr@quickPlotGrobList

    # Section 3 - the actual Plotting
    # Plot each element passed to Plot function, one at a time
    for (subPlots in names(quickSubPlots)) {
      quickPlotGrobCounter <- 0
      for (sGrob in quickSubPlots[[subPlots]]) {
        quickPlotGrobCounter <- quickPlotGrobCounter + 1
        needPlot <- updated$needPlotting[[subPlots]][[quickPlotGrobCounter]]

        if (needPlot) {
          isNewPlot <- updated$isNewPlot[[subPlots]][[quickPlotGrobCounter]]
          isReplot <- updated$isReplot[[subPlots]][[quickPlotGrobCounter]]
          isBaseSubPlot <- updated$isBaseLayer[[subPlots]][[quickPlotGrobCounter]]

          whPlotFrame <- match(sGrob@plotName, names(quickSubPlots))

          # Check that the extents are equal.
          # If not, then x and y axes are written where necessary.
          xyAxes <- .xyAxes(sGrob, arr = updated$curr@arr, whPlotFrame)

          layerFromPlotObj <- (names(newQuickPlots@quickPlotGrobList) %in%
                                  sGrob@plotName)
          whLayerFromPO <- which(layerFromPlotObj)

          objNames <- unique(unname(unlist(lapply(newQuickPlots@quickPlotGrobList,
                                                  function(x) x[[1]]@objName))))
          whPlotObj <- which(objNames %in% sGrob@objName)


          layerFromPlotObj <- if (length(whLayerFromPO) == 0) {
            FALSE
          } else if (isQuickPlotLong[whLayerFromPO]) {
            FALSE
          } else {
            layerFromPlotObj[whLayerFromPO]
          }
          grobToPlot <- .identifyGrobToPlot(sGrob, plotObjs[whPlotObj], layerFromPlotObj)

          isPlotFnAddable <- if (!is(grobToPlot, ".quickPlotObjects")) {
            if (is(grobToPlot, ".quickPlot")) {
              FALSE
            } else if (sGrob@plotArgs$userProvidedPlotFn & !isTRUE(grobToPlot[["add"]])) {
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }

          if (sGrob@plotArgs$new | is(grobToPlot, "igraph") |
             is(grobToPlot, "histogram") | isPlotFnAddable) {
            ## draw a white rectangle to clear plot
            sGrob <- .refreshGrob(sGrob, subPlots, legendRange,
                                  grobToPlot, plotArgs = sGrob@plotArgs,
                                  nColumns = updated$curr@arr@columns,
                                  whLayerFromPO)
            wipe <- TRUE # can't overplot a histogram
          } else {
            wipe <- FALSE
          }

          sGrob <- .updateGrobGPTextAxis(sGrob, arr = updated$curr@arr, newArr = newArr)

          if (is(grobToPlot, "spatialObjects")) {
            zMat <- .convertSpatialToPlotGrob(grobToPlot, sGrob, layerFromPlotObj,
                                              arr = updated$curr@arr, newArr,
                                              quickPlotGrobCounter, subPlots, cols)
          } else {
            zMat <- NULL
          }

          # SpatialPointsDataFrame could have a column named color
          if (is(grobToPlot, "SpatialPointsDataFrame")) {
            if (any(grepl(pattern = "color", colnames(grobToPlot))) & is.null(cols))
              sGrob@plotArgs$cols <- getColors(grobToPlot)
          }

          # Add legendRange if not provided
          if (is(grobToPlot, "Raster")) {
            if (is.null(sGrob@plotArgs$legendRange)) {
              sGrob@plotArgs$legendRange <-
                c(zMat$minz, zMat$maxz)
          }}

          # Plot any grobToPlot to device, given all the parameters
          sGrob <- .Plot(sGrob, grobToPlot, subPlots, quickSubPlots, quickPlotGrobCounter,
                         isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                         vps, nonPlotArgs)

        } # needPlot
        updated$isNewPlot[[subPlots]][[quickPlotGrobCounter]] <- FALSE
        updated$curr@quickPlotGrobList[[subPlots]][[quickPlotGrobCounter]]@plotArgs <-
          sGrob@plotArgs
      } # sGrob
    } # subPlots

    seekViewport("top", recording = FALSE)
    .assignQuickPlot(paste0("quickPlot", dev.cur()), updated)
    return(invisible(updated$curr))
})

################################################################################
#' Re-plot to a specific device
#'
#' @param toDev    Numeric. Which device should the new rePlot be plotted to.
#'                 Default is current device.
#'
#' @param fromDev  Numeric. Which device should the replot information be taken from.
#'                 Default is current device
#'
#' @param clearFirst Logical. Should \code{clearPlot} be run before replotting. Default TRUE.
#'
#' @export
#' @include plotting-classes.R
#' @importFrom grDevices dev.cur
#' @rdname Plot
#'
rePlot <- function(toDev = dev.cur(), fromDev = dev.cur(), clearFirst = TRUE, ...) {
  if (exists(paste0("quickPlot", fromDev), envir = .quickPlotEnv)) {
    currQuickPlots <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    dev(toDev)
    if (clearFirst) clearPlot(toDev)
    suppressWarnings(Plot(currQuickPlots$curr,
                          new = rep(TRUE, length(currQuickPlots$curr@arr@names)), ...))
  } else {
    stop(
      paste(
        "Nothing to rePlot. Need to call Plot first,",
        "or change to correct active device with dev(x),",
        "where x is the active device number."
      )
    )
  }
}