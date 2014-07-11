### R code from vignette source 'genomic-plot.Rnw'

###################################################
### code chunk number 1: genomic-plot.Rnw:52-55 (eval = FALSE)
###################################################
## bed = generateRandomBed()
## bed = generateRandomBed(nr = 200, nc = 4)
## bed = generateRandomBed(fun = function(k) runif(k))


###################################################
### code chunk number 2: genomic-plot.Rnw:64-65 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram()


###################################################
### code chunk number 3: genomic-plot.Rnw:72-78 (eval = FALSE)
###################################################
## cytoband.file = paste(system.file(package = "circlize"),
##     "/extdata/cytoBand.txt", sep = "")
## circos.initializeWithIdeogram(cytoband.file)
## cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
##     "numeric", "character", "character"), sep = "\t")
## circos.initializeWithIdeogram(cytoband.df)


###################################################
### code chunk number 4: genomic-plot.Rnw:89-91 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(species = "hg18")
## circos.initializeWithIdeogram(species = "mm10")


###################################################
### code chunk number 5: genomic-plot.Rnw:98-99 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))


###################################################
### code chunk number 6: genomic-plot.Rnw:109-117 (eval = FALSE)
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
### code chunk number 7: genomic-plot.Rnw:127-131 (eval = FALSE)
###################################################
## cytoband = read.cytoband()
## cytoband = read.cytoband(file)
## cytoband = read.cytoband(df)
## cytoband = read.cytoband(species)


###################################################
### code chunk number 8: genomic-plot.Rnw:138-140 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(plotType = c("axis", "labels"))
## circos.initializeWithIdeogram(plotType = NULL)


###################################################
### code chunk number 9: genomic-plot.Rnw:145-152 (eval = FALSE)
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
### code chunk number 11: genomic-plot.Rnw:179-188 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(plotType = NULL)
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     chr = get.cell.meta.data("sector.index")
##     xlim = get.cell.meta.data("xlim")
##     ylim = get.cell.meta.data("ylim")
##     circos.rect(xlim[1], 0, xlim[2], 0.5,
##         col = rgb(runif(1), runif(1), runif(1)))
##     circos.text(mean(xlim), 0.9, chr, cex = 0.5, facing = "clockwise")
## }, bg.border = NA)


###################################################
### code chunk number 12: figcustomizeideogram
###################################################
source("src/genomic-02-customize_ideogram.R")


###################################################
### code chunk number 13: genomic-plot.Rnw:212-217 (eval = FALSE)
###################################################
## df = data.frame(
##     name  = c("TP53",  "TP63",    "TP73"),
##     start = c(7565097, 189349205, 3569084),
##     end   = c(7590856, 189615068, 3652765))
## circos.genomicInitialize(df)


###################################################
### code chunk number 14: genomic-plot.Rnw:226-232 (eval = FALSE)
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
### code chunk number 16: genomic-plot.Rnw:258-261 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 17: genomic-plot.Rnw:276-284 (eval = FALSE)
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
### code chunk number 18: genomic-plot.Rnw:298-302 (eval = FALSE)
###################################################
## circos.genomicPoints(region, value, ...)
## circos.genomicPoints(region, value, numeric.column = c(1, 2))
## circos.genomicPoints(region, value, cex, pch)
## circos.genomicPoints(region, value, sector.index, track.index)


###################################################
### code chunk number 19: genomic-plot.Rnw:315-320 (eval = FALSE)
###################################################
## circos.genomicLines(region, value, ...)
## circos.genomicLines(region, value, numeric.column = c(1, 2))
## circos.genomicLines(region, value, lwd, lty = "segment")
## circos.genomicLines(region, value, area, baseline, border)
## circos.genomicLines(region, value, sector.index, track.index)


###################################################
### code chunk number 20: genomic-plot.Rnw:332-336 (eval = FALSE)
###################################################
## circos.genomicText(region, value, ...)
## circos.genomicText(region, value, y = 1, labels = "gene")
## circos.genomicText(region, value, direction, adj)
## circos.genomicText(region, value, sector.index, track.index)


###################################################
### code chunk number 21: genomic-plot.Rnw:345-348 (eval = FALSE)
###################################################
## circos.genomicRect(region, value, ytop = 1, ybottom = 0)
## circos.genomicRect(region, value, ytop.column = 2, ybottom = 0)
## circos.genomicRect(region, value, col, border)


###################################################
### code chunk number 22: genomic-plot.Rnw:357-358
###################################################
library(circlize)


###################################################
### code chunk number 23: genomic-plot.Rnw:361-363
###################################################
col_fun = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))


###################################################
### code chunk number 24: genomic-plot.Rnw:380-387 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, numeric.column = 4, 
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
##         circos.genomicPoints(region, value)
##         circos.genomicPoints(region, value, numeric.column = 1)
## })


###################################################
### code chunk number 25: genomic-plot.Rnw:396-407 (eval = FALSE)
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
### code chunk number 26: genomic-plot.Rnw:426-432 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, stack = TRUE,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })


###################################################
### code chunk number 27: genomic-plot.Rnw:443-449 (eval = FALSE)
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
### code chunk number 29: genomic-plot.Rnw:479-490 (eval = FALSE)
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
### code chunk number 30: genomic-plot.Rnw:499-501 (eval = FALSE)
###################################################
## circos.genomicLink(bed1, bed2)
## circos.genomicLink(bed1, bed2, col)


###################################################
### code chunk number 31: figgenomiclink
###################################################
source("src/genomic-05-genomicLink.R")


###################################################
### code chunk number 32: genomic-plot.Rnw:520-524 (eval = FALSE)
###################################################
## highlight.chromosome("chr1")
## highlight.chromosome("chr1", track.index = c(2, 3))
## highlight.chromosome("chr1", col = NA, border = "red")
## highlight.chromosome("chr1", padding = c(0.1, 0.1, 0.1, 0.1))


###################################################
### code chunk number 33: fighighlight
###################################################
source("src/genomic-06-highlight.chromosome.R")


###################################################
### code chunk number 34: genomic-plot.Rnw:562-565 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, posTransform = posTransform.default, ...)
## })


###################################################
### code chunk number 35: genomic-plot.Rnw:571-576 (eval = FALSE)
###################################################
## circos.genomicPosTransformLines(data, posTransform = posTransform.default)
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     horizontalLine = "top")
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     type = "reverse")


###################################################
### code chunk number 36: figpostransform
###################################################
source("src/genomic-07-posTransformLines.R")


###################################################
### code chunk number 37: genomic-plot.Rnw:598-601 (eval = FALSE)
###################################################
## circos.genomicDensity(bed)
## circos.genomicDensity(bed, window.size = 1e6)
## circos.genomicDensity(bedlist)


###################################################
### code chunk number 38: genomic-plot.Rnw:609-611 (eval = FALSE)
###################################################
## circos.genoimcRainfall(bed)
## circos.genoimcRainfall(bedlist, col = c("red", "green"))


###################################################
### code chunk number 39: figrainfall
###################################################
source("src/genomic-08-rainfallplot.R")


