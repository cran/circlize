### R code from vignette source 'draw-ideogram.Rnw'

###################################################
### code chunk number 1: figideogram1
###################################################
library(circlize)
d = read.table(file = paste(system.file(package = "circlize"),
        "/extdata/cytoBand.txt", sep=""),
    colClasses = c("factor", "numeric", "numeric", "factor", "factor"))
head(d)


###################################################
### code chunk number 2: figideogram2
###################################################
chromosome = levels(d[[1]])
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
### code chunk number 3: figideogram3 (eval = FALSE)
###################################################
## xlim = matrix(nrow = 0, ncol = 2)
## for(chr in chromosome) {
##     d2 = d[d[[1]] == chr, ]
##     xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
## }


###################################################
### code chunk number 4: figideogram4 (eval = FALSE)
###################################################
## par(mar = c(1, 1, 1, 1), lwd = 0.5)
## circos.par("cell.padding" = c(0, 0, 0, 0))


###################################################
### code chunk number 5: figideogram5 (eval = FALSE)
###################################################
## circos.initialize(factors = factor(chromosome, levels = chromosome),
##     xlim = xlim)


###################################################
### code chunk number 6: figideogram6 (eval = FALSE)
###################################################
## circos.trackPlotRegion(factors = chromosome, ylim = c(0, 1),
##     bg.border = NA, track.height = 0.1)


###################################################
### code chunk number 7: figideogram7 (eval = FALSE)
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
##     circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr,
##         border = "black")
##         
##     # axis
##     major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 50000000)
##     circos.axis(h = 0.5, major.at = major.at,
##         labels = paste(major.at/1000000, "MB", sep = ""),
##         sector.index = chr, labels.cex = 0.3)
##     cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
##     
##     # chromosome names, only the number part or the letter part
##     circos.text(mean(cell.xlim), 1.2, labels = gsub("chr", "", chr),
##         sector.index = chr, cex = 0.8)
## }


###################################################
### code chunk number 8: figideogram8 (eval = FALSE)
###################################################
## circos.link(sector.index1 = "chr2", point1 = 111111111,
##             sector.index2 = "chr16", point2 = 55555555)


###################################################
### code chunk number 9: figideogram9 (eval = FALSE)
###################################################
## # create a new track
## circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
## circos.text(88888888, 0.2, labels = "site", sector.index = "chr6",
##     adj = c(0.5, 1))
## circos.lines(c(88888888, 88888888), c(0.3, 1), sector.index = "chr6",
##     straight = TRUE)


###################################################
### code chunk number 10: figideogram
###################################################
xlim = matrix(nrow = 0, ncol = 2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
}
par(mar = c(1, 1, 1, 1), lwd = 0.5)
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(factors = factor(chromosome, levels = chromosome),
    xlim = xlim)
circos.trackPlotRegion(factors = chromosome, ylim = c(0, 1),
    bg.border = NA, track.height = 0.1)
for(chr in chromosome) {
    # data in `chr`
    d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    
    # assign colors
    col = rep("#FFFFFF", n)
    col[d2[[5]] == "gpos100"] = rgb(0, 0, 0, maxColorValue = 255)
    col[d2[[5]] == "gpos"]    = rgb(0, 0, 0, maxColorValue = 255)
    col[d2[[5]] == "gpos75"]  = rgb(130, 130, 130, maxColorValue = 255)
    col[d2[[5]] == "gpos66"]  = rgb(160, 160, 160, maxColorValue = 255)
    col[d2[[5]] == "gpos50"]  = rgb(200, 200, 200, maxColorValue = 255)
    col[d2[[5]] == "gpos33"]  = rgb(210, 210, 210, maxColorValue = 255)
    col[d2[[5]] == "gpos25"]  = rgb(200, 200, 200, maxColorValue = 255)
    col[d2[[5]] == "gvar"]    = rgb(220, 220, 220, maxColorValue = 255)
    col[d2[[5]] == "gneg"]    = rgb(255, 255, 255, maxColorValue = 255)
    col[d2[[5]] == "acen"]    = rgb(217, 47, 39, maxColorValue = 255)
    col[d2[[5]] == "stalk"]   = rgb(100, 127, 164, maxColorValue = 255)
    
    # rectangles for different locus
    for(i in seq_len(n)) {
        circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
            col = col[i], border = NA)
    }
    # rectangle that cover the whole chromosome
    circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr,
        border = "black")
        
    # axis
    major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 50000000)
    circos.axis(h = 0.5, major.at = major.at,
        labels = paste(major.at/1000000, "MB", sep = ""),
        sector.index = chr, labels.cex = 0.3)
    cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
    
    # chromosome names, only the number part or the letter part
    circos.text(mean(cell.xlim), 1.2, labels = gsub("chr", "", chr),
        sector.index = chr, cex = 0.8)
}
circos.link(sector.index1 = "chr2", point1 = 111111111,
            sector.index2 = "chr16", point2 = 55555555)
# create a new track
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
circos.text(88888888, 0.2, labels = "site", sector.index = "chr6",
    adj = c(0.5, 1))
circos.lines(c(88888888, 88888888), c(0.3, 1), sector.index = "chr6",
    straight = TRUE)


###################################################
### code chunk number 11: figgenomic (eval = FALSE)
###################################################
## library(circlize)
## set.seed(12345)
## 
## circos.initializeWithIdeogram()
## 
## circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.2,
##     panel.fun = function(x, y) {
##         xlim = get.cell.meta.data("xlim")
##         xrange = get.cell.meta.data("xrange")
## 
##         x1 = xlim[1] + runif(5)*xrange
##         x1 = sort(x1)
##         x2 = seq(xlim[1], xlim[2], length.out = 5)
##         for(i in 1:5) {
##             circos.lines(c(x1[i], x2[i]), c(1, 0.5), straight = TRUE)
##             circos.text(x2[i], 0.4, labels = "gene", adj = c(0, 0.5),
##                 cex = 0.4, direction = "vertical_left")
##         }
##     })
## 
## 
## circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, bg.col ="#EEEEEE",
##     track.height = 0.1, panel.fun = function(x, y) {
##         xlim = get.cell.meta.data("xlim")
##         ylim = get.cell.meta.data("ylim")
##         for(i in -2:2) {
##             circos.lines(xlim, c(i, i)/2, col = "#999999", lwd = 0.2)
##         }
##         xrange = get.cell.meta.data("xrange")
##         x = NULL
##         y = NULL
##         for(i in 1:5) {
##             
##             x2 = seq(xlim[1] + (i-1)/5*xrange, xlim[1] + (i)/5*xrange, by = 1000000)
##             x = c(x, x2)
##             y = c(y, runif(length(x2))^2*sample(c(-1, 1), 1))
##         }
##         col = ifelse(y > 0, "#E41A1C", "#4DAF4A")
##         circos.points(x, y, col = col, cex = 0.2, pch = 16)
##     })
## 
## circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, track.height = 0.1,
##     panel.fun = function(x, y) {
##         xlim = get.cell.meta.data("xlim")
##         xrange = get.cell.meta.data("xrange")
##         x = seq(xlim[1], xlim[2], by = 10000000)
##         y = runif(length(x))
##         circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FF7F00")
##         y = -runif(length(x))
##         circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FFFF33")
##     })
## 
## 
## circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
##     panel.fun = function(x, y) {
##         xlim = get.cell.meta.data("xlim")
##         xrange = get.cell.meta.data("xrange")
##         x = seq(xlim[1], xlim[2], by = 10000000)
##         for(i in seq_along(x)) {
##             if(i == 1) {
##                 next
##             }
##             circos.rect(x[i-1], 0, x[i], 1, col = rgb(runif(1), runif(1), runif(1)), 
##                 border = NA)
##         }
##     })
## 
## chromosome = paste("chr", c(1:22, "X", "Y"), sep = "")
## for(i in 1:50) {
##     chr = sample(chromosome, 2)
##     xlim1 = get.cell.meta.data("xlim", sector.index = chr[1], track.index = 1)
##     xrange1 = get.cell.meta.data("xrange", sector.index = chr[1], track.index = 1)
##     xlim2 = get.cell.meta.data("xlim", sector.index = chr[2], track.index = 1)
##     xrange2 = get.cell.meta.data("xrange", sector.index = chr[2], track.index = 1)
##     
##     r = runif(2)
##     if(abs(r[1] - r[2]) < 0.2) {
##         x1 = c(xlim1[1] + r[1]*xrange1, xlim1[1] + r[2]*xrange1)
##     } else {
##         x1 = c(xlim1[1] + r[1]*xrange1)
##     }
##     
##     r = runif(2)
##     if(abs(r[1] - r[2]) < 0.2) {
##         x2 = c(xlim2[1] + r[1]*xrange2, xlim2[1] + r[2]*xrange2)
##     } else {
##         x2 = c(xlim2[1] + r[1]*xrange2)
##     }
##     
##     
##     circos.link(chr[1], x1, chr[2], x2, col = sample(c('#9E0142', '#D53E4F', '#F46D43',
##           '#FDAE61', '#FEE08B', '#FFFFBF', '#E6F598',
##           '#ABDDA4', '#66C2A5', '#3288BD', '#5E4FA2'), 1))
## }
## 
## degree = get.cell.meta.data("xplot", sector.index = "chr1", track.index = 1)
## start.degree = degree[1]
## end.degree = degree[2]
## rou1 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 1)[2]
## rou2 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 5)[1]
## 
## draw.sector(center = c(0, 0), start.degree = start.degree, end.degree = end.degree,
##             rou1 = rou1+0.05, rou2 = rou2-0.01, col = "#FF000020", border = NA)
## circos.clear()


###################################################
### code chunk number 12: figgenomic
###################################################
library(circlize)
set.seed(12345)

circos.initializeWithIdeogram()

circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.2,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")

        x1 = xlim[1] + runif(5)*xrange
        x1 = sort(x1)
        x2 = seq(xlim[1], xlim[2], length.out = 5)
        for(i in 1:5) {
            circos.lines(c(x1[i], x2[i]), c(1, 0.5), straight = TRUE)
            circos.text(x2[i], 0.4, labels = "gene", adj = c(0, 0.5),
                cex = 0.4, direction = "vertical_left")
        }
    })


circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, bg.col ="#EEEEEE",
    track.height = 0.1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        for(i in -2:2) {
            circos.lines(xlim, c(i, i)/2, col = "#999999", lwd = 0.2)
        }
        xrange = get.cell.meta.data("xrange")
        x = NULL
        y = NULL
        for(i in 1:5) {
            
            x2 = seq(xlim[1] + (i-1)/5*xrange, xlim[1] + (i)/5*xrange, by = 1000000)
            x = c(x, x2)
            y = c(y, runif(length(x2))^2*sample(c(-1, 1), 1))
        }
        col = ifelse(y > 0, "#E41A1C", "#4DAF4A")
        circos.points(x, y, col = col, cex = 0.2, pch = 16)
    })

circos.trackPlotRegion(ylim = c(-1, 1), bg.border = NA, track.height = 0.1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        y = runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FF7F00")
        y = -runif(length(x))
        circos.lines(x, y, area = TRUE, area.baseline = 0, border = NA, col = "#FFFF33")
    })


circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.05,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xrange = get.cell.meta.data("xrange")
        x = seq(xlim[1], xlim[2], by = 10000000)
        for(i in seq_along(x)) {
            if(i == 1) {
                next
            }
            circos.rect(x[i-1], 0, x[i], 1, col = rgb(runif(1), runif(1), runif(1)), 
                border = NA)
        }
    })

chromosome = paste("chr", c(1:22, "X", "Y"), sep = "")
for(i in 1:50) {
    chr = sample(chromosome, 2)
    xlim1 = get.cell.meta.data("xlim", sector.index = chr[1], track.index = 1)
    xrange1 = get.cell.meta.data("xrange", sector.index = chr[1], track.index = 1)
    xlim2 = get.cell.meta.data("xlim", sector.index = chr[2], track.index = 1)
    xrange2 = get.cell.meta.data("xrange", sector.index = chr[2], track.index = 1)
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x1 = c(xlim1[1] + r[1]*xrange1, xlim1[1] + r[2]*xrange1)
    } else {
        x1 = c(xlim1[1] + r[1]*xrange1)
    }
    
    r = runif(2)
    if(abs(r[1] - r[2]) < 0.2) {
        x2 = c(xlim2[1] + r[1]*xrange2, xlim2[1] + r[2]*xrange2)
    } else {
        x2 = c(xlim2[1] + r[1]*xrange2)
    }
    
    
    circos.link(chr[1], x1, chr[2], x2, col = sample(c('#9E0142', '#D53E4F', '#F46D43',
          '#FDAE61', '#FEE08B', '#FFFFBF', '#E6F598',
          '#ABDDA4', '#66C2A5', '#3288BD', '#5E4FA2'), 1))
}

degree = get.cell.meta.data("xplot", sector.index = "chr1", track.index = 1)
start.degree = degree[1]
end.degree = degree[2]
rou1 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 1)[2]
rou2 = get.cell.meta.data("yplot", sector.index = "chr1", track.index = 5)[1]

draw.sector(center = c(0, 0), start.degree = start.degree, end.degree = end.degree,
            rou1 = rou1+0.05, rou2 = rou2-0.01, col = "#FF000020", border = NA)
circos.clear()


###################################################
### code chunk number 13: figtwo (eval = FALSE)
###################################################
## 
## library(circlize)
## circos.clear()
## 
## circos.initializeWithIdeogram()
## circos.link("chr1", 12345678, "chr1", 87654321, top.ratio = 0.8)
## circos.link("chr1", 22222222, "chr1", 99999999, top.ratio = 0.8)
## circos.clear()
## 
## d = read.table(file = paste(system.file(package = "circlize"),
##     "/extdata/cytoBand.txt", sep=""),
##     colClasses = c("factor", "numeric", "numeric", "factor", "factor"))
## 
## chromosome = c("chr1")
##     
## xlim = matrix(nrow = 0, ncol = 2)
## for(chr in chromosome) {
##     d2 = d[d[[1]] == chr, ]
##     xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
## }
##     
## circos.clear()
## par(mar = c(1, 1, 1, 1), new = TRUE)
##     
## circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2),
##     clock.wise = FALSE, start.degree = -90)
## circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
## circos.trackPlotRegion(factors = factor(chromosome, levels = chromosome),
##     ylim = c(0, 1), bg.border = NA, track.height = 0.2)
## for(chr in chromosome) {
##     d2 = d[d[[1]] == chr, ]
##     n = nrow(d2)
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
##     for(i in seq_len(n)) {
##         circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
##             col = col[i], border = NA)
##     }
##     circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
##     major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 10000000)
##     circos.axis(h = 0.5, major.at = major.at, 
##         labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, 
##         labels.cex = 0.4, labels.direction = "vertical_left")
##     cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
##     circos.text(cell.xlim[1] + mean(cell.xlim), -0.5, labels = chr, 
##         sector.index = chr, cex = 0.8)
##     circos.link("chr1", 12345678, "chr1", 87654321)
##     circos.link("chr1", 22222222, "chr1", 99999999)
## }
## circos.clear()       


###################################################
### code chunk number 14: figtwo
###################################################

library(circlize)
circos.clear()

circos.initializeWithIdeogram()
circos.link("chr1", 12345678, "chr1", 87654321, top.ratio = 0.8)
circos.link("chr1", 22222222, "chr1", 99999999, top.ratio = 0.8)
circos.clear()

d = read.table(file = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""),
    colClasses = c("factor", "numeric", "numeric", "factor", "factor"))

chromosome = c("chr1")
    
xlim = matrix(nrow = 0, ncol = 2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    xlim = rbind(xlim,c(min(d2[[2]]), max(d2[[3]])))
}
    
circos.clear()
par(mar = c(1, 1, 1, 1), new = TRUE)
    
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2),
    clock.wise = FALSE, start.degree = -90)
circos.initialize(factor(chromosome, levels = chromosome), xlim = xlim)
circos.trackPlotRegion(factors = factor(chromosome, levels = chromosome),
    ylim = c(0, 1), bg.border = NA, track.height = 0.2)
for(chr in chromosome) {
    d2 = d[d[[1]] == chr, ]
    n = nrow(d2)
    col = rep("#FFFFFF", n)
    col[d2[[5]] == "gpos100"] = rgb(0, 0, 0, maxColorValue = 255)
    col[d2[[5]] == "gpos"]    = rgb(0, 0, 0, maxColorValue = 255)
    col[d2[[5]] == "gpos75"]  = rgb(130, 130, 130, maxColorValue = 255)
    col[d2[[5]] == "gpos66"]  = rgb(160, 160, 160, maxColorValue = 255)
    col[d2[[5]] == "gpos50"]  = rgb(200, 200, 200, maxColorValue = 255)
    col[d2[[5]] == "gpos33"]  = rgb(210, 210, 210, maxColorValue = 255)
    col[d2[[5]] == "gpos25"]  = rgb(200, 200, 200, maxColorValue = 255)
    col[d2[[5]] == "gvar"]    = rgb(220, 220, 220, maxColorValue = 255)
    col[d2[[5]] == "gneg"]    = rgb(255, 255, 255, maxColorValue = 255)
    col[d2[[5]] == "acen"]    = rgb(217, 47, 39, maxColorValue = 255)
    col[d2[[5]] == "stalk"]   = rgb(100, 127, 164, maxColorValue = 255)
    for(i in seq_len(n)) {
        circos.rect(d2[i, 2], 0, d2[i, 3], 0.4, sector.index = chr,
            col = col[i], border = NA)
    }
    circos.rect(d2[1, 2], 0, d2[n, 3], 0.4, sector.index = chr, border = "black")
    major.at = seq(0, 10^nchar(max(xlim[, 2])), by = 10000000)
    circos.axis(h = 0.5, major.at = major.at, 
        labels = paste(major.at/1000000, "MB", sep = ""), sector.index = chr, 
        labels.cex = 0.4, labels.direction = "vertical_left")
    cell.xlim = get.cell.meta.data("xlim", sector.index = chr)
    circos.text(cell.xlim[1] + mean(cell.xlim), -0.5, labels = chr, 
        sector.index = chr, cex = 0.8)
    circos.link("chr1", 12345678, "chr1", 87654321)
    circos.link("chr1", 22222222, "chr1", 99999999)
}
circos.clear()       


