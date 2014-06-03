### R code from vignette source 'draw-genomic-plot.Rnw'

###################################################
### code chunk number 1: draw-genomic-plot.Rnw:52-55 (eval = FALSE)
###################################################
## bed = generateRandomBed()
## bed = generateRandomBed(nr = 200, nc = 4)
## bed = generateRandomBed(fun = function(k) runif(k))


###################################################
### code chunk number 2: draw-genomic-plot.Rnw:64-65 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram()


###################################################
### code chunk number 3: draw-genomic-plot.Rnw:71-77 (eval = FALSE)
###################################################
## cytoband.file = paste(system.file(package = "circlize"),
##     "/extdata/cytoBand.txt", sep = "")
## circos.initializeWithIdeogram(cytoband.file)
## cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
##     "numeric", "character", "character"), sep = "\t")
## circos.initializeWithIdeogram(cytoband.df)


###################################################
### code chunk number 4: draw-genomic-plot.Rnw:88-90 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(species = "hg18")
## circos.initializeWithIdeogram(species = "mm10")


###################################################
### code chunk number 5: draw-genomic-plot.Rnw:97-98 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))


###################################################
### code chunk number 6: draw-genomic-plot.Rnw:108-114 (eval = FALSE)
###################################################
## cytoband = read.table(cytoband.file)
## circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
## cytoband[[1]] = factor(cytoband[[1]], levels = paste("chr", c(22:1, "X", "Y")))
## circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
## cytoband = read.table(cytoband.file)
## circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)


###################################################
### code chunk number 7: draw-genomic-plot.Rnw:124-128 (eval = FALSE)
###################################################
## cytoband = read.cytoband()
## cytoband = read.cytoband(file)
## cytoband = read.cytoband(df)
## cytoband = read.cytoband(species)


###################################################
### code chunk number 8: draw-genomic-plot.Rnw:135-137 (eval = FALSE)
###################################################
## circos.initializeWithIdeogram(plotType = c("axis", "labels"))
## circos.initializeWithIdeogram(plotType = NULL)


###################################################
### code chunk number 9: draw-genomic-plot.Rnw:142-147 (eval = FALSE)
###################################################
## circos.par("start.degree" = 90)
## circos.initializeWithIdeogram()
## 
## circos.par("gap.degree" = rep(c(2, 4), 11))
## circos.initializeWithIdeogram()


###################################################
### code chunk number 10: draw-genomic-plot.Rnw:173-178 (eval = FALSE)
###################################################
## df = data.frame(
##     name  = c("TP53",  "TP63",    "TP73"),
##     start = c(7565097, 189349205, 3569084),
##     end   = c(7590856, 189615068, 3652765))
## circos.genomicInitialize(df)


###################################################
### code chunk number 11: draw-genomic-plot.Rnw:187-188 (eval = FALSE)
###################################################
## circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))


###################################################
### code chunk number 12: draw-genomic-plot.Rnw:211-214 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 13: draw-genomic-plot.Rnw:229-237 (eval = FALSE)
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
### code chunk number 14: draw-genomic-plot.Rnw:251-255 (eval = FALSE)
###################################################
## circos.genomicPoints(region, value, ...)
## circos.genomicPoints(region, value, numeric.column = c(1, 2))
## circos.genomicPoints(region, value, cex, pch)
## circos.genomicPoints(region, value, sector.index, track.index)


###################################################
### code chunk number 15: draw-genomic-plot.Rnw:268-273 (eval = FALSE)
###################################################
## circos.genomicLines(region, value, ...)
## circos.genomicLines(region, value, numeric.column = c(1, 2))
## circos.genomicLines(region, value, lwd, lty = "segment")
## circos.genomicLines(region, value, area, area.baseline, border)
## circos.genomicLines(region, value, sector.index, track.index)


###################################################
### code chunk number 16: draw-genomic-plot.Rnw:285-289 (eval = FALSE)
###################################################
## circos.genomicText(region, value, ...)
## circos.genomicText(region, value, y = 1, labels = "gene")
## circos.genomicText(region, value, direction, adj)
## circos.genomicText(region, value, sector.index, track.index)


###################################################
### code chunk number 17: draw-genomic-plot.Rnw:298-300 (eval = FALSE)
###################################################
## circos.genomicRect(region, value, ytop = 1, ybottom = 0)
## circos.genomicRect(region, value, col, border)


###################################################
### code chunk number 18: draw-genomic-plot.Rnw:309-310
###################################################
library(circlize)


###################################################
### code chunk number 19: draw-genomic-plot.Rnw:313-315
###################################################
col_fun = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))


###################################################
### code chunk number 20: draw-genomic-plot.Rnw:332-338 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, ...)
##     circos.genomicPoints(region, value)
##     circos.genomicPoints(region, value, numeric.column = 1)
## })


###################################################
### code chunk number 21: draw-genomic-plot.Rnw:345-351 (eval = FALSE)
###################################################
## bedlist = list(generateRandomBed(), generateRandomBed())
## circos.genomicTrackPlotRegion(bedlist,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })


###################################################
### code chunk number 22: draw-genomic-plot.Rnw:369-375 (eval = FALSE)
###################################################
## bed = generateRandomBed(nc = 2)
## circos.genomicTrackPlotRegion(bed, stack = TRUE,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, col = i, ...)
## })


###################################################
### code chunk number 23: draw-genomic-plot.Rnw:385-391 (eval = FALSE)
###################################################
## bedlist = list(generateRandomBed(), generateRandomBed())
## circos.genomicTrackPlotRegion(bedlist, stack = TRUE,
##     panel.fun = function(region, value, ...) {
##         i = getI(...)
##         circos.genomicPoints(region, value, ...)
## })


###################################################
### code chunk number 24: draw-genomic-plot.Rnw:418-428 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(bed, ylim = c(-1, 1),
##     panel.fun = function(region, value, ...) {
##         circos.genomicPoints(region, value, ...)
##         cell.xlim = get.cell.meta.data("cell.xlim")
##         for(h in c(-1, -0.5, 0, 0.5, 1)) {
##             circos.lines(cell.xlim, c(0, 0), lty = 2, col = "grey")
##         }
##         circos.text(x, y, labels)
##         circos.axis("top")
## })


###################################################
### code chunk number 25: draw-genomic-plot.Rnw:437-439 (eval = FALSE)
###################################################
## circos.genomicLink(bed1, bed2)
## circos.genomicLink(bed1, bed2, col)


###################################################
### code chunk number 26: draw-genomic-plot.Rnw:455-459 (eval = FALSE)
###################################################
## highlight.chromosome("chr1")
## highlight.chromosome("chr1", track.index = c(2, 3))
## highlight.chromosome("chr1", col = NA, border = "red")
## highlight.chromosome("chr1", padding = c(0.1, 0.1, 0.1, 0.1))


###################################################
### code chunk number 27: draw-genomic-plot.Rnw:491-494 (eval = FALSE)
###################################################
## circos.genomicTrackPlotRegion(data, panel.fun = function(region, value, ...) {
##     circos.genomicPoints(region, value, posTransform = posTransform.default, ...)
## })


###################################################
### code chunk number 28: draw-genomic-plot.Rnw:500-505 (eval = FALSE)
###################################################
## circos.genomicPosTransformLines(data, posTransform = posTransform.default)
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     horizontalLine = "top")
## circos.genomicPosTransformLines(data, posTransform = posTransform.default,
##     type = "reverse")


###################################################
### code chunk number 29: draw-genomic-plot.Rnw:523-526 (eval = FALSE)
###################################################
## circos.genomicDensity(bed)
## circos.genomicDensity(bed, window.size = 1e6)
## circos.genomicDensity(bedlist)


###################################################
### code chunk number 30: draw-genomic-plot.Rnw:534-536 (eval = FALSE)
###################################################
## circos.genoimcRainfall(bed)
## circos.genoimcRainfall(bedlist, col = c("red", "green"))


