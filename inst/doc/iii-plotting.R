## ----initial-clearPlot, eval=TRUE, echo=FALSE, fig.keep='none', message=FALSE----
quickPlot::clearPlot()

## ----load_files, eval=TRUE, echo=TRUE, message=FALSE, fig.height=2------------
#  Make list of maps from package database to load, and what functions to use to load them
library(data.table)
library(terra)
library(quickPlot)

# omit forestAge and percentPine maps for simplicity
files <- system.file("maps", package = "quickPlot") |>
  dir(full.names = TRUE, pattern = "tif")
filelist <- data.frame(file = files[-c(2, 5) ], stringsAsFactors = FALSE)
print(filelist)

# Load files to memory (using rasterToMemory), assign to a simList we call maps 
maps <- lapply(filelist$file, function(x) rast(x))
names(maps) <- sapply(basename(filelist$file), function(x) {
  strsplit(x, split = "\\.")[[1]][1]
})

# put into a single stack object in the simulation environment for ease of use below
landscape <- c(maps$DEM, maps$forestCover, maps$habitatQuality)

## ----first_plot, eval=TRUE, echo=TRUE, fig.height=2---------------------------
Plot(landscape, new = TRUE)
# make a SpatialPoints object
caribou <- terra::vect(cbind(x = stats::runif(1e2, -50, 50),
                             y = stats::runif(1e2, -50, 50)))
Plot(caribou)
Plot(caribou, addTo = "landscape$habitatQuality")

# from ?vect help file
x1 <- rbind(c(-18,-2), c(-14,5.5), c(1, 0), c(-14,-6))
x2 <- rbind(c(-1,0), c(14,6), c(16,0), c(14,-5.5))
x3 <- rbind(c(-12.5,0), c(0,6), c(4,0.5), c(1.5,-4.5))
hole <- rbind(c(8,0), c(10.5,1.3), c(12,0.2), c(10.5,-1.3))
z <- rbind(cbind(object=1, part=1, x1, hole=0), cbind(object=2, part=1, x3, hole=0),
			cbind(object=3, part=1, x2, hole=0), cbind(object=3, part=1, hole, hole=1))
colnames(z)[3:4] <- c('x', 'y')
spP <- terra::vect(z, "polygons")

Plot(spP)
Plot(spP, addTo = "landscape$habitatQuality", gp = gpar(lwd = 2))



# from ?vect help file
z[z[, "hole"]==1, "object"] <- 4
sl <- vect(z[,1:4], "lines")

Plot(sl, gp = gpar(col = c("red", "blue"), lwd = 2), addTo = "landscape$DEM")

## ----mixing_layer_types, eval=TRUE, echo=TRUE, fig.height=5-------------------
clearPlot()
Plot(landscape, caribou, maps$DEM, spP, axes = TRUE,
     gp = gpar(cex = 0.5), visualSqueeze = 0.65)

## ----ggplot, eval=TRUE, echo=TRUE, cache=FALSE, fig.height=2------------------
if (requireNamespace("ggplot2")) {
  ggObj <- data.frame(x = stats::rnorm(1e3)) |> ggplot2::ggplot(ggplot2::aes(x = x)) +
    ggplot2::geom_histogram()
  clearPlot()
  Plot(caribou, axes = "L", new = TRUE) 
  Plot(ggObj) 
}

## ----base-objects, eval=FALSE, echo=TRUE, cache=FALSE, fig.height=2-----------
#  baseObj <- rnorm(1e3)
#  baseObj2 <- baseObj * 1.2 + rnorm(1e3)
#  clearPlot()
#  # Plot(baseObj, axes = "L", ylab = "Something meaningful")
#  Plot(baseObj, baseObj2, addTo = "scatterplot", axes = TRUE)
#  newPoints <- rnorm(10)
#  newPoints2 <- newPoints * 1.2 + rnorm(10)
#  Plot(newPoints, newPoints2, addTo = "scatterplot", col = "red")

## ----set_colours, eval=TRUE, echo=TRUE, fig.height=2--------------------------
  
  # can change colour palette
  clearPlot()
  Plot(landscape) # original
  
  # can use RColorBrewer if installed
  habQualCols <- if (requireNamespace("RColorBrewer")) {
    RColorBrewer::brewer.pal(9, "Spectral")
  } else {
    colorRampPalette(c("blue", "purple"))(9)
  }
  
  mapColours <- list(
    DEM = topo.colors(50),
    forestCover = colorRampPalette(c("blue", "orange", "purple", "red"))(50),
    habitatQuality = habQualCols
  )
  setColors(landscape, n = 50) <- mapColours
  Plot(landscape, new = TRUE) # oh, how pretty!

## ----gp_gpAxis_gpText, eval=TRUE, echo=TRUE, fig.height=2---------------------
clearPlot()
Plot(caribou, gpAxis = gpar(cex = 0.4), size = 1)
Plot(maps$DEM, gpText = gpar(cex = 0.4))
clearPlot()
Plot(maps$DEM, caribou, gpText = list(gpar(cex = 2), gpar(cex = 0.1)), new = TRUE)

## ----visualSqueeze, eval=TRUE, echo=TRUE, fig.height=2------------------------
# x axis gets cut off in pdf and html
clearPlot()
Plot(maps$DEM)
clearPlot()
Plot(maps$DEM, visualSqueeze = 0.6, new = TRUE)

## ----legendRange, eval=TRUE, echo=TRUE, fig.height=2--------------------------
clearPlot()
Plot(maps$DEM, legendRange = c(0, 500), new = TRUE)

## ----zoomExtent, eval=TRUE, echo=TRUE, fig.height=2---------------------------
Plot(maps$DEM, zoomExtent = terra::ext(c(-1, 10, -1, 20)), new = TRUE)

## ----arrows, eval=TRUE, echo=TRUE, fig.height=2-------------------------------
clearPlot()
Plot(maps$DEM)
Plot(sl, addTo = "maps$DEM", length = 0.2)

## ----new-as-list--------------------------------------------------------------
clearPlot()
Plot(landscape)
Plot(landscape, new = list(TRUE, FALSE, TRUE), axes = "L")

## ----simple_add, eval=TRUE, echo=TRUE, fig.height=3---------------------------
clearPlot()
Plot(landscape)
# can add a new plot to the plotting window
Plot(caribou, new = FALSE, axes = FALSE)

## ----add_with_rearrangement, eval=TRUE, echo=TRUE, fig.height=2---------------
clearPlot()
Plot(landscape)
# can add a new plot to the plotting window
Plot(caribou, new = FALSE, axes = FALSE)

## ----add_with_same_name, eval=TRUE, echo=TRUE, fig.height=2-------------------
clearPlot()
Plot(landscape)
landscape$forestCover[] = ((landscape$forestCover[] + 10) %% 30)
# can add a new plot to the plotting window
Plot(landscape, new = FALSE)
# note: zeros are treated as no colour by default.
# if this is not the correct behaviour, can use `zero.color`
Plot(landscape, new = FALSE, zero.color = "blue")

## ----speedup, eval=TRUE, echo=TRUE, fig.height=2------------------------------
system.time(Plot(landscape, caribou, maps$DEM, new = TRUE))
system.time(Plot(landscape, caribou, maps$DEM, speedup = 10, new = TRUE))
# can add a new plot to the plotting window

## ----add, eval=TRUE, echo=TRUE, fig.height=2----------------------------------
clearPlot()
Plot(landscape)
Plot(caribou, addTo = "landscape$DEM", size = 2)

## ----clearPlot, eval=TRUE, echo=TRUE, fig.height=2----------------------------
clearPlot()
Plot(caribou)

## ----clickValues, eval=FALSE, echo=TRUE---------------------------------------
#  clearPlot()
#  Plot(landscape)
#  clickValues(3) # click at three locations on the Plot device

## ----clickExtent, eval=FALSE, echo=TRUE---------------------------------------
#  clearPlot()
#  Plot(landscape)
#  clickExtent() # click at two locations on the Plot device

## ----rePlot, eval=FALSE, echo=TRUE, cache=FALSE-------------------------------
#  rePlot()
#  rePlot(4)
#  rePlot(visualSqueeze = 1, axes = FALSE)
#  rePlot(visualSqueeze = 0.7, axes = FALSE, cols = "Reds")

## ----Plot a .quickPlot object, eval=FALSE, echo=TRUE, cache=FALSE-------------
#  clearPlot()
#  plots <- Plot(landscape)
#  
#  # change values
#  landscape$forestCover[landscape$habitatQuality > 0.9] <- 0
#  
#  Plot(plots)
#  # same as:
#  rePlot()
#  
#  # but can be combined with other objects
#  Plot(caribou, plots)
#  Plot(caribou, plots, sl)

