### R code from vignette source 'draw-ideogram.Rnw'

###################################################
### code chunk number 1: draw-ideogram.Rnw:34-39
###################################################
library(circlize)
d = read.table(file = paste(system.file(package = "circlize"),
        "/extdata/cytoBand.txt", sep=""),
    colClasses = c("character", "numeric", "numeric", "character", "character"))
head(d)


###################################################
### code chunk number 2: draw-ideogram.Rnw:56-67
###################################################
chromosome = unique(d[[1]])
chromosome.ind = gsub("chr", "", chromosome)
chromosome.num = grep("^\\d+$", chromosome.ind, value = TRUE)
chromosome.letter = chromosome.ind[!grepl("^\\d+$", chromosome.ind)]
chromosome.num = sort(as.numeric(chromosome.num))
chromosome.letter = sort(chromosome.letter)
chromosome.num = paste("chr", chromosome.num, sep = "")
chromosome.letter = paste("chr", chromosome.letter, sep = "")

chromosome = c(chromosome.num, chromosome.letter)
chromosome


###################################################
### code chunk number 3: draw-ideogram.Rnw:77-82 (eval = FALSE)
###################################################
## xlim = matrix(nrow = 0, ncol = 2)
## for(chr in chromosome) {
##     d2 = d[d[[1]] == chr, ]
##     xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
## }


###################################################
### code chunk number 4: draw-ideogram.Rnw:89-93 (eval = FALSE)
###################################################
## cytoband = read.cytoband()  # by default, it reads human cytoband data
## d = cytoband$df
## chromosome = cytoband$chromosome
## xlim = cbind(rep(0, length(chromosome)), cytoband$chr.len)


###################################################
### code chunk number 5: draw-ideogram.Rnw:99-101 (eval = FALSE)
###################################################
## par(mar = c(1, 1, 1, 1), lwd = 0.5)
## circos.par("cell.padding" = c(0, 0, 0, 0))


###################################################
### code chunk number 6: draw-ideogram.Rnw:116-117 (eval = FALSE)
###################################################
## circos.initialize(factors = factor(chromosome, levels = chromosome), xlim = xlim)


###################################################
### code chunk number 7: draw-ideogram.Rnw:128-130 (eval = FALSE)
###################################################
## circos.trackPlotRegion(factors = chromosome, ylim = c(0, 1), bg.border = NA, 
##     track.height = 0.1)


###################################################
### code chunk number 8: draw-ideogram.Rnw:154-192 (eval = FALSE)
###################################################
## for(chr in chromosome) {
##     # data in `chr`
##     d2 = d[d[[1]] == chr, ]
##     n = nrow(d2)
##     
##     # assign colors
##     col = rep("#FFFFFF", n)
##     col[d2[[5]] == "gpos100"] = rgb(0, 0, 0, maxColorValue = 255)
##     col[d2[[5]] == "gpos"]    = rgb(0, 0, 0, maxColorValue = 255)
##     col[d2[[5]] == "gpos75"]  = rgb(130, 130, 130, maxColorValue = 255)
##     col[d2[[5]] == "gpos66"]  = rgb(160, 160, 160, maxColorValue = 255)
##     col[d2[[5]] == "gpos50"]  = rgb(200, 200, 200, maxColorValue = 255)
##     col[d2[[5]] == "gpos33"]  = rgb(210, 210, 210, maxColorValue = 255)
##     col[d2[[5]] == "gpos25"]  = rgb(200, 200, 200, maxColorValue = 255)
##     col[d2[[5]] == "gvar"]    = rgb(220, 220, 220, maxColorValue = 255)
##     col[d2[[5]] == "gneg"]    = rgb(255, 255, 255, maxColorValue = 255)
##     col[d2[[5]] == "acen"]    = rgb(217, 47, 39, maxColorValue = 255)
##     col[d2[[5]] == "stalk"]   = rgb(100, 127, 164, maxColorValue = 255)
##     
##     # rectangles for different locus
##     for(i in seq_len(n)) {
##         circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
##             col = col[i], border = NA)
##     }
##     # rectangle that cover the whole chromosome
##     circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
##         
##     # axis
##     major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 50000000)
##     circos.axis(h = 0.5, major.at = major.at,
##         labels = paste(major.at/1000000, "MB", sep = ""),
##         sector.index = chr, labels.cex = 0.3)
##     chr.xlim = get.cell.meta.data("xlim", sector.index = chr)
##     
##     # chromosome names, only the number part or the letter part
##     circos.text(mean(chr.xlim), 1.2, labels = gsub("chr", "", chr),
##         sector.index = chr, cex = 0.8)
## }


###################################################
### code chunk number 9: draw-ideogram.Rnw:208-210 (eval = FALSE)
###################################################
## circos.link(sector.index1 = "chr2", point1 = 111111111, sector.index2 = "chr16", 
##     point2 = 55555555)


###################################################
### code chunk number 10: draw-ideogram.Rnw:218-222 (eval = FALSE)
###################################################
## # create a new track
## circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
## circos.text(88888888, 0.2, labels = "site", sector.index = "chr6", adj = c(0.5, 1))
## circos.lines(c(88888888, 88888888), c(0.3, 1), sector.index = "chr6", straight = TRUE)


###################################################
### code chunk number 11: figideogram
###################################################
source("src/ideogram-01-simple.R")


###################################################
### code chunk number 12: figgenomic
###################################################
source("src/ideogram-02-genomic.R")


###################################################
### code chunk number 13: figtwo
###################################################
source("src/ideogram-03-nested.R")


###################################################
### code chunk number 14: figzoom
###################################################
source("src/ideogram-04-zoom.R")


