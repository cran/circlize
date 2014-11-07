### R code from vignette source 'genomic_plot.Rnw'

###################################################
### code chunk number 1: genomic_plot.Rnw:52-55 (eval = FALSE)
###################################################
## bed = generateRandomBed()
## bed = generateRandomBed(nr = 200, nc = 4)
## bed = generateRandomBed(fun = function(k) runif(k))


###################################################
### code chunk number 2: genomic_plot.Rnw:64-65 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram()


###################################################
### code chunk number 3: genomic_plot.Rnw:72-78 (eval = FALSE)
###################################################
## cytoband.file = paste(system.file(package = "circlize"),
##     "/extdata/cytoBand.txt", sep = "")
## circos.initializeWithIdeogram(cytoband.file)
## cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
##     "numeric", "character", "character"), sep = "\t")
## circos.initializeWithIdeogram(cytoband.df)


###################################################
### code chunk number 4: genomic_plot.Rnw:89-91 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(species = "hg18")
## circos.initializeWithIdeogram(species = "mm10")


###################################################
### code chunk number 5: genomic_plot.Rnw:98-99 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))


###################################################
### code chunk number 6: genomic_plot.Rnw:109-117 (eval = FALSE)
###################################################
## cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
##     "numeric", "character", "character"), sep = "\t")
## circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
## cytoband[[1]] = factor(cytoband[[1]], levels = paste("chr", c(22:1, "X", "Y")))
## circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
## cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
##     "numeric", "character", "character"), sep = "\t")
## circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)


###################################################
### code chunk number 7: genomic_plot.Rnw:127-131 (eval = FALSE)
###################################################
## cytoband = read.cytoband()
## cytoband = read.cytoband(file)
## cytoband = read.cytoband(df)
## cytoband = read.cytoband(species)


###################################################
### code chunk number 8: genomic_plot.Rnw:138-143 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(plotType = c("axis", "labels"))
## circos.initializeWithIdeogram(plotType = NULL)
## # height of these pre-defined tracks can be set
## circos.initializeWithIdeogram(track.height = 0.05)
## circos.initializeWithIdeogram(ideogram.height = 0.05)


###################################################
### code chunk number 9: genomic_plot.Rnw:148-155 (eval = FALSE)
###################################################
## circos.par("start.degree" = 90)
## circos.initializeWithIdeogram()
## circos.clear()
## 
## circos.par("gap.degree" = rep(c(2, 4), 11))
## circos.initializeWithIdeogram()
## circos.clear()


###################################################
### code chunk number 10: figinitializewithideogram
###################################################
source("src/genomic-01-circos.initializeWithIdeogram.R")


###################################################
### code chunk number 11: genomic_plot.Rnw:182-191 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(plotType = NULL)
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     chr = get.cell.meta.data("sector.index")
##     xlim = get.cell.meta.data("xlim")
##     ylim = get.cell.meta.data("ylim")
##     circos.rect(xlim[1], 0, xlim[2], 0.5,
##         col = rgb(runif(1), runif(1), runif(1)))
##     circos.text(mean(xlim), 0.9, chr, cex = 0.5, facing = "clockwise", niceFacing = TRUE)
## }, bg.border = NA)


###################################################
### code chunk number 12: figcustomizeideogram
###################################################
source("src/genomic-02-customize_ideogram.R")


###################################################
### code chunk number 13: genomic_plot.Rnw:215-220 (eval = FALSE)
###################################################
## df = data.frame(
##     name  = c("TP53",  "TP63",    "TP73"),
##     start = c(7565097, 189349205, 3569084),
##     end   = c(7590856, 189615068, 3652765))
## circos.genomicInitialize(df)


###################################################
### code chunk number 14: genomic_plot.Rnw:229-235 (eval = FALSE)
###################################################
## circos.genomicInitialize(df)
## circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))
## circos.genomicInitialize(df, plotType)
## 
## circos.par(gap.degree = 2)
## circos.genomicInitialize(df)


###################################################
### code chunk number 15: figgenomicinitialize
###################################################
source("src/genomic-03-TPgenemodel.R")


###################################################
### code chunk number 16: genomic_plot.Rnw:261-264 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 17: genomic_plot.Rnw:279-287 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, ylim = c(0, 1),
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
## })
## circos.genomicTrackPlotRegion(data, numeric.column, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 18: genomic_plot.Rnw:301-305 (eval = FALSE)
###################################################
## circos.genomicPoints(region, value, ...)
## circos.genomicPoints(region, value, numeric.column = c(1, 2))
## circos.genomicPoints(region, value, cex, pch)
## circos.genomicPoints(region, value, sector.index, track.index)


###################################################
### code chunk number 19: genomic_plot.Rnw:318-323 (eval = FALSE)
###################################################
## circos.genomicLines(region, value, ...)
## circos.genomicLines(region, value, numeric.column = c(1, 2))
## circos.genomicLines(region, value, lwd, lty = "segment")
## circos.genomicLines(region, value, area, baseline, border)
## circos.genomicLines(region, value, sector.index, track.index)


###################################################
### code chunk number 20: genomic_plot.Rnw:335-340 (eval = FALSE)
###################################################
## circos.genomicText(region, value, ...)
## circos.genomicText(region, value, y, labels)
## circos.genomicText(region, value, numeric.column, labels.column)
## circos.genomicText(region, value, facing, niceFacing, adj)
## circos.genomicText(region, value, sector.index, track.index)


###################################################
### code chunk number 21: genomic_plot.Rnw:349-352 (eval = FALSE)
###################################################
## circos.genomicRect(region, value, ytop = 1, ybottom = 0)
## circos.genomicRect(region, value, ytop.column = 2, ybottom = 0)
## circos.genomicRect(region, value, col, border)


###################################################
### code chunk number 22: genomic_plot.Rnw:361-362
###################################################
library(circlize)


###################################################
### code chunk number 23: genomic_plot.Rnw:365-367
###################################################
col_fun = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))


###################################################
### code chunk number 24: genomic_plot.Rnw:384-391 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, numeric.column = 4, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
##         circos.genomicPoints(region, value)
##         circos.genomicPoints(region, value, numeric.column = 1)
## })


###################################################
### code chunk number 25: genomic_plot.Rnw:400-411 (eval = FALSE)
###################################################
## bedlist = list(generateRandomBed(), generateRandomBed())
## circos.genomicTrackPlotRegion(bedlist,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })
## circos.genomicTrackPlotRegion(bedlist, numeric.column = c(4, 5),
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })


###################################################
### code chunk number 26: genomic_plot.Rnw:430-436 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, stack = TRUE,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })


###################################################
### code chunk number 27: genomic_plot.Rnw:447-453 (eval = FALSE)
###################################################
## bedlist = list(generateRandomBed(), generateRandomBed())
## circos.genomicTrackPlotRegion(bedlist, stack = TRUE,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 28: figgenomicpoints
###################################################
source("src/genomic-04-genomicTrackPlotRegion.R")


###################################################
### code chunk number 29: genomic_plot.Rnw:480-535 (eval = FALSE)
###################################################
## ### track A
## bed = generateRandomBed(nr = 300)
## circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
## })
## 
## ### track B
## bed = generateRandomBed(nr = 300)
## circos.genomicTrackPlotRegion(bed, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
##         i = getI(...)
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
## })
## 
## ### track C
## bed1 = generateRandomBed(nr = 300)
## bed2 = generateRandomBed(nr = 300)
## bed_list = list(bed1, bed2)
## circos.genomicTrackPlotRegion(bed_list, 
##     panel.fun = function(region, value, ...) {
##         cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
##         i = getI(...)
##         circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
## })
## 
## ### track D
## circos.genomicTrackPlotRegion(bed_list, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
##         i = getI(...)
##         circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
## })
## 
## ### track E
## bed = generateRandomBed(nr = 300, nc = 4)
## circos.genomicTrackPlotRegion(bed, 
##     panel.fun = function(region, value, ...) {
##         cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
##         circos.genomicPoints(region, value, cex = 0.5, pch = 16, col = 1:4, ...)
## })
## 
## ### track F
## bed = generateRandomBed(nr = 300, nc = 4)
## circos.genomicTrackPlotRegion(bed, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
##         i = getI(...)
##         circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
## })


###################################################
### code chunk number 30: genomic_plot.Rnw:540-585 (eval = FALSE)
###################################################
## ### track A
## bed = generateRandomBed(nr = 500)
## circos.genomicTrackPlotRegion(bed, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicLines(region, value, type = "l", ...)
## })
## 
## ### track B
## bed1 = generateRandomBed(nr = 500)
## bed2 = generateRandomBed(nr = 500)
## bed_list = list(bed1, bed2)
## circos.genomicTrackPlotRegion(bed_list, 
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicLines(region, value, col = i, ...)
## })
## 
## ### track C
## circos.genomicTrackPlotRegion(bed_list, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicLines(region, value, col = i, ...)
## })
## 
## ### track D
## bed = generateRandomBed(nr = 500, nc = 4)
## circos.genomicTrackPlotRegion(bed, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicLines(region, value, col = 1:4, ...)
## })
## 
## ### track E
## bed = generateRandomBed(nr = 500, nc = 4)
## circos.genomicTrackPlotRegion(bed, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicLines(region, value, col = i, ...)
## })
## 
## ### track F
## bed = generateRandomBed(nr = 200)
## circos.genomicTrackPlotRegion(bed, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
## })


###################################################
### code chunk number 31: genomic_plot.Rnw:590-626 (eval = FALSE)
###################################################
## ### track A
## f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
## bed = generateRandomBed(nr = 100, nc = 4)
## circos.genomicTrackPlotRegion(bed, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicRect(region, value, col = f(value[[1]]), 
##             border = f(value[[1]]), ...)
## })
## 
## ### track B
## bed1 = generateRandomBed(nr = 100)
## bed2 = generateRandomBed(nr = 100)
## bed_list = list(bed1, bed2)
## circos.genomicTrackPlotRegion(bed_list, stack = TRUE, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicRect(region, value, col = f(value[[1]]), 
##             border = f(value[[1]]), ...)
## })
## 
## ### track C
## circos.genomicTrackPlotRegion(bed_list, ylim = c(0, 3), 
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicRect(region, value, ytop = i+0.4, ybottom = i-0.4, 
##             col = f(value[[1]]), border = f(value[[1]]), ...)
## })
## 
## ### track D
## bed = generateRandomBed(nr = 200)
## circos.genomicTrackPlotRegion(bed, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicRect(region, value, ytop.column = 1, ybottom = 0, 
##             col = ifelse(value[[1]] > 0, "red", "green"), ...)
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         circos.lines(cell.xlim, c(0, 0), lty = 2, col = "#00000040")
## })


###################################################
### code chunk number 32: genomic_plot.Rnw:634-645 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(bed, ylim = c(-1, 1),
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
## 
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         for(h in c(-1, -0.5, 0, 0.5, 1)) {
##             circos.lines(cell.xlim, c(0, 0), lty = 2, col = "grey")
##         }
##         circos.text(x, y, labels)
##         circos.axis("top")
## })


###################################################
### code chunk number 33: genomic_plot.Rnw:654-656 (eval = FALSE)
###################################################
## circos.genomicLink(bed1, bed2)
## circos.genomicLink(bed1, bed2, col)


###################################################
### code chunk number 34: figgenomiclink
###################################################
source("src/genomic-05-genomicLink.R")


###################################################
### code chunk number 35: genomic_plot.Rnw:675-679 (eval = FALSE)
###################################################
## highlight.chromosome("chr1")
## highlight.chromosome("chr1", track.index = c(2, 3))
## highlight.chromosome("chr1", col = NA, border = "red")
## highlight.chromosome("chr1", padding = c(0.1, 0.1, 0.1, 0.1))


###################################################
### code chunk number 36: fighighlight
###################################################
source("src/genomic-06-highlight.chromosome.R")


###################################################
### code chunk number 37: genomic_plot.Rnw:719-722 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, posTransform = posTransform.default, ...)
## })


###################################################
### code chunk number 38: genomic_plot.Rnw:730-735 (eval = FALSE)
###################################################
## circos.genomicPosTransformLines(data, posTransform = posTransform.default)
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     horizontalLine = "top")
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     direction = "outside")


###################################################
### code chunk number 39: figpostransform
###################################################
source("src/genomic-07-posTransformLines.R")


###################################################
### code chunk number 40: genomic_plot.Rnw:760-767 (eval = FALSE)
###################################################
## bed = generateRandomBed(nr = 400, fun = function(k) rep("text", k))
## circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), 
##     panel.fun = function(region, value, ...) {
##         circos.genomicText(region, value, y = 0, labels.column = 1, 
##         facing = "clockwise", adj = c(0, 0.5), posTransform = posTransform.text, 
##         cex = 0.8)
## }, track.height = 0.1, bg.border = NA)


###################################################
### code chunk number 41: genomic_plot.Rnw:776-782 (eval = FALSE)
###################################################
## i_track = get.cell.meta.data("track.index")  # previous track
## circos.genomicPosTransformLines(bed, 
##     posTransform = function(region, value) posTransform.text(region, y = 0, 
##         labels = value[[1]], cex = 0.8, track.index = i_track), 
##     direction = "outside"
## )


###################################################
### code chunk number 42: genomic_plot.Rnw:791-806 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), track.height = 0.1, bg.border = NA)
## i_track = get.cell.meta.data("track.index")  # remember this empty track, we'll come back soon
## 
## circos.genomicTrackPlotRegion(bed, ylim = c(0, 1),
##     panel.fun = function(region, value, ...) {
##         circos.genomicText(region, value, y = 1, labels.column = 1, 
##         facing = "clockwise", adj = c(1, 0.5),
##         posTransform = posTransform.text, cex = 0.8)
## }, track.height = 0.1, bg.border = NA)
## 
## circos.genomicPosTransformLines(bed, 
##     posTransform = function(region, value) posTransform.text(region, y = 1, 
##         labels = value[[1]], cex = 0.8, track.index = i_track+1),
##     direction = "inside", track.index = i_track
## )


###################################################
### code chunk number 43: genomic_plot.Rnw:812-825 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), 
##     panel.fun = function(region, value, ...) {
##         circos.genomicText(region, value, y = 0, labels.column = 1, 
##         facing = "clockwise", adj = c(0, 0.5), posTransform = posTransform.text, 
##         cex = 0.8, padding = 0.2)
## }, track.height = 0.1, bg.border = NA)
## 
## i_track = get.cell.meta.data("track.index")  # previous track
## circos.genomicPosTransformLines(bed, 
##     posTransform = function(region, value) posTransform.text(region, y = 0, 
##         labels = value[[1]], cex = 0.8, padding = 0.2, track.index = i_track),
##     direction = "outside"
## )


###################################################
### code chunk number 44: figpostransformtext
###################################################
source("src/genomic-07-posTransformLinesText.R")


###################################################
### code chunk number 45: figrainfall
###################################################
source("src/genomic-08-rainfallplot.R")


###################################################
### code chunk number 46: genomic_plot.Rnw:861-864 (eval = FALSE)
###################################################
## circos.genomicDensity(bed)
## circos.genomicDensity(bed, window.size = 1e6)
## circos.genomicDensity(bedlist)


###################################################
### code chunk number 47: genomic_plot.Rnw:872-874 (eval = FALSE)
###################################################
## circos.genoimcRainfall(bed)
## circos.genoimcRainfall(bedlist, col = c("red", "green"))


