### R code from vignette source 'circlize.Rnw'

###################################################
### code chunk number 1: figcorrespondence
###################################################
source("src/intro-00-correspondence.R")


###################################################
### code chunk number 2: circlize.Rnw:101-105 (eval = FALSE)
###################################################
## set.seed(12345)
## n = 1000
## a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
##     x = rnorm(n), y = runif(n))


###################################################
### code chunk number 3: circlize.Rnw:116-120 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
## circos.par("default.track.height" = 0.1)
## circos.initialize(factors = a$factor, x = a$x)


###################################################
### code chunk number 4: circlize.Rnw:145-154 (eval = FALSE)
###################################################
## circos.trackPlotRegion(factors = a$factor, y = a$y,
##     panel.fun = function(x, y) {
##         circos.axis()
## })
## col = rep(c("#FF0000", "#00FF00"), 4)
## circos.trackPoints(a$factor, a$x, a$y, col = col,
##     pch = 16, cex = 0.5)
## circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
## circos.text(1, 0.5, "right", sector.index = "a")


###################################################
### code chunk number 5: circlize.Rnw:160-162 (eval = FALSE)
###################################################
## bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
## circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)


###################################################
### code chunk number 6: circlize.Rnw:170-178 (eval = FALSE)
###################################################
## circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
##   panel.fun = function(x, y) {
##       grey = c("#FFFFFF", "#CCCCCC", "#999999")
##       i = get.cell.meta.data("sector.numeric.index")
##       circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
##       circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
##       circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
##   })


###################################################
### code chunk number 7: circlize.Rnw:186-188 (eval = FALSE)
###################################################
## circos.updatePlotRegion(sector.index = "d", track.index = 2)
## circos.points(x = -2:2, y = rep(0, 5))


###################################################
### code chunk number 8: circlize.Rnw:193-195 (eval = FALSE)
###################################################
## circos.trackPlotRegion(factors = a$factor, y = a$y)
## circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")


###################################################
### code chunk number 9: circlize.Rnw:201-205 (eval = FALSE)
###################################################
## circos.link("a", 0, "b", 0, top.ratio = 0.9)
## circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
##     border = "blue", top.ratio = 0.2)
## circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)


###################################################
### code chunk number 10: circlize.Rnw:210-211 (eval = FALSE)
###################################################
## circos.clear()


###################################################
### code chunk number 11: figexample
###################################################
source("src/intro-01-glance.R")


###################################################
### code chunk number 12: figtransformation
###################################################
source("src/intro-03-transformation.R")


###################################################
### code chunk number 13: figorder
###################################################
source("src/intro-02-order.R")


###################################################
### code chunk number 14: figcoordinate
###################################################
source("src/intro-04-coordinate.R")


###################################################
### code chunk number 15: figfactor
###################################################
source("src/intro-05-factor.R")


###################################################
### code chunk number 16: figsectorwidth
###################################################
source("src/intro-05-sectorwidth.R")


###################################################
### code chunk number 17: figregion
###################################################
source("src/intro-06-region.R")


###################################################
### code chunk number 18: figdirection
###################################################
source("src/intro-07-direction.R")


###################################################
### code chunk number 19: figlines
###################################################
source("src/intro-08-lines.R")


###################################################
### code chunk number 20: figlinecurve
###################################################
source("src/intro-08-linescurve.R")


###################################################
### code chunk number 21: figtext
###################################################
source("src/intro-09-text.R")


###################################################
### code chunk number 22: figerrorline
###################################################
source("src/intro-10-smooth.R")


###################################################
### code chunk number 23: figaxis
###################################################
source("src/intro-11-axis.R")


###################################################
### code chunk number 24: figlink
###################################################
source("src/intro-12-link.R")


###################################################
### code chunk number 25: figadjlink
###################################################
source("src/intro-13-adjlink.R")


###################################################
### code chunk number 26: fighist
###################################################
source("src/intro-14-hist.R")


###################################################
### code chunk number 27: figheatmap
###################################################
source("src/intro-15-heatmap.R")


###################################################
### code chunk number 28: sectorhighlight
###################################################
source("src/intro-16-highlight.R")


###################################################
### code chunk number 29: circlize.Rnw:988-1019 (eval = FALSE)
###################################################
## library(circlize)
## factors = sample(letters[1:6], 100, replace = TRUE)
## x = rnorm(100)
## y = rnorm(100)
## 
## par(mar = c(1, 1, 1, 1))
## circos.initialize(factors = factors, x = x)
## circos.trackPlotRegion(factors = factors, x = x, y = y, bg.col = "#EEEEEE",
##     bg.border = NA, track.height = 0.4, panel.fun = function(x, y) {
##     
##     cell.xlim = get.cell.meta.data("cell.xlim")
##     cell.ylim = get.cell.meta.data("cell.ylim")
##     # reference lines
##     for(xi in seq(cell.xlim[1], cell.xlim[2], length.out = 10)) {
##         circos.lines(c(xi, xi), cell.ylim, lty = 2, col = "white") 
##     }
##     for(yi in seq(cell.ylim[1], cell.ylim[2], length.out = 5)) {
##         circos.lines(cell.xlim, c(yi, yi), lty = 2, col = "white") 
##     }
##     
##     xlim = get.cell.meta.data("xlim")
##     ylim = get.cell.meta.data("ylim")
##     circos.rect(xlim[1], 1, xlim[2], ylim[2], col = "#FF000020", border = NA)
##     circos.rect(xlim[1], ylim[1], xlim[2], -1, col = "#00FF0020", border = NA)
## 
##     circos.points(x[y >= 1], y[y >= 1], pch = 16, cex = 0.8, col = "red")
##     circos.points(x[y <= -1], y[y <= -1], pch = 16, cex = 0.8, col = "green")
##     circos.points(x[y > -1 & y < 1], y[y > -1 & y < 1], pch = 16, cex = 0.5)
## })
## 
## circos.clear()


###################################################
### code chunk number 30: combine
###################################################
source("src/intro-16-combine.R")


###################################################
### code chunk number 31: circlize.Rnw:1040-1055 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1))
## circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1),
##     "clock.wise" = FALSE, "gap.degree" = 0)
## factors = letters[1:4]
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
## circos.updatePlotRegion(sector.index = "a", bg.border = "black")
## x1 = runif(100)
## y1 = runif(100)
## circos.points(x1, y1, pch = 16, cex = 0.5)
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
## circos.updatePlotRegion(sector.index = "a", bg.border = "black")
## circos.lines(1:100/100, y1, pch = 16, cex = 0.5)
## circos.clear()


###################################################
### code chunk number 32: figpart
###################################################
source("src/intro-17-part.R")


###################################################
### code chunk number 33: circlize.Rnw:1075-1094 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1))
## factors = letters[1:4]
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = NA, bg.border = NA)
## circos.updatePlotRegion(sector.index = "a", bg.border = "black")
## x1 = runif(100)
## y1 = runif(100)
## circos.points(x1, y1, pch = 16, cex = 0.5)
## 
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1),bg.col = NA, bg.border = NA)
## circos.updatePlotRegion(sector.index = "a", bg.border = "black")
## x1 = runif(100)
## y1 = runif(100)
## circos.points(x1, y1, pch = 16, cex = 0.5)
## 
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1))
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1))
## circos.clear()


###################################################
### code chunk number 34: figpart2
###################################################
source("src/intro-18-part2.R")


###################################################
### code chunk number 35: circlize.Rnw:1116-1133 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1))
## factors = letters[1:4]
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     circos.text(0.5, 0.5, "outer circos")
## })
## circos.clear()
## 
## par(new = TRUE)
## circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2))
## factors = letters[1:3]
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     circos.text(0.5, 0.5, "inner circos")
## })
## circos.clear()


###################################################
### code chunk number 36: fignested
###################################################
source("src/intro-19-nested.R")


###################################################
### code chunk number 37: circlize.Rnw:1152-1175 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1))
## factors = letters[1:4]
## circos.par("canvas.xlim" = c(-1, 1.5), "canvas.ylim" = c(-1, 1.5), start.degree = -45)
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
## circos.updatePlotRegion(sector.index = "a")
## circos.text(0.5, 0.5, "first one")
## circos.updatePlotRegion(sector.index = "b")
## circos.text(0.5, 0.5, "first one")
## 
## circos.clear()
## 
## par(new = TRUE)
## circos.par("canvas.xlim" = c(-1.5, 1), "canvas.ylim" = c(-1.5, 1), start.degree = -45)
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
## circos.updatePlotRegion(sector.index = "d")
## circos.text(0.5, 0.5, "second one")
## circos.updatePlotRegion(sector.index = "c")
## circos.text(0.5, 0.5, "second one")
## 
## circos.clear()


###################################################
### code chunk number 38: figseperated
###################################################
source("src/intro-20-seperated.R")


###################################################
### code chunk number 39: circlize.Rnw:1195-1210 (eval = FALSE)
###################################################
## library(circlize)
## par(mar = c(1, 1, 1, 1))
## factors = letters[1:4]
## lim = c(1, 1.1, 1.2, 1.3)
## for(i in 1:4) {
##     circos.par("canvas.xlim" = c(-lim[i], lim[i]),
##         "canvas.ylim" = c(-lim[i], lim[i]), "default.track.height" = 0.4)
##     circos.initialize(factors = factors, xlim = c(0, 1))
##     circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA)
##     circos.updatePlotRegion(sector.index = factors[i], bg.border = "black")
##     circos.points(runif(10), runif(10), pch = 16)
##     circos.clear()
##     par(new = TRUE)
## }
## par(new = FALSE)


###################################################
### code chunk number 40: figdifrad
###################################################
source("src/intro-21-diffradius.R")


###################################################
### code chunk number 41: circlize.Rnw:1234-1252 (eval = FALSE)
###################################################
## library(circlize)
## set.seed(12345)
## par(mar = c(1, 1, 1, 1))
## factors = letters[1:4]
## circos.par("canvas.xlim" = c(-1.5, 1.5), "canvas.ylim" = c(-1.5, 1.5), "gap.degree" = 10)
## circos.initialize(factors = factors, xlim = c(0, 1))
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     circos.points(1:20/20, 1:20/20)
## })
## circos.lines(c(1/20, 0.5), c(1/20, 3), sector.index = "d", straight = TRUE)
## circos.text(0.5, 3, "mark", sector.index = "d", adj = c(0.5, 0))
## 
## circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
##     circos.points(1:20/20, 1:20/20)
## })
## text(0, 0, "this is\nthe center", cex = 1.5)
## legend("bottomleft", pch = 1, legend = "this is the legend")
## circos.clear()


###################################################
### code chunk number 42: figoutside
###################################################
source("src/intro-22-outside.R")


###################################################
### code chunk number 43: circlize.Rnw:1273-1296 (eval = FALSE)
###################################################
## library(circlize)
## 
## set.seed(12345)
## rand_color = function() {
##     return(rgb(runif(1), runif(1), runif(1)))
## }
## 
## layout(matrix(1:9, 3, 3))
## for(i in 1:9) {
##     factors = 1:8
##     par(mar = c(0.5, 0.5, 0.5, 0.5))
##     circos.par(cell.padding = c(0, 0, 0, 0))
##     circos.initialize(factors, xlim = c(0, 1))
##     circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.05,
##         bg.col = sapply(1:8, function(x) rand_color()), bg.border = NA)
##     for(i in 1:20) {
##         se = sample(1:8, 2)
##         col = rand_color()
##         col = paste(col, "40", sep = "")
##         circos.link(se[1], runif(2), se[2], runif(2), col = col)
##     }
##     circos.clear()
## }


###################################################
### code chunk number 44: figlayout
###################################################
source("src/intro-23-layout.R")


