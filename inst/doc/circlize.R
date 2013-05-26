### R code from vignette source 'circlize.Rnw'

###################################################
### code chunk number 1: figexample
###################################################
library(circlize)
set.seed(12345)
n = 10000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))
for(le in levels(a$factor)) {
    a$x[a$factor == le] = a$x[a$factor == le] * runif(1)
}

par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1, points.overflow.warning = FALSE)
circos.initialize(factors = a$factor, x = a$x)

bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
col = rep(c("#FF000010", "#00FF0010"), 4)
circos.trackPlotRegion(factors = a$factor, y = a$y, panel.fun = function(x, y) {
    circos.axis()
})
circos.trackPoints(a$factor, a$x, a$y, col = col, pch = 16, cex = 0.5)
circos.text(-1,0.5, "left", sector.index = "a")
circos.text(1,0.5, "right", sector.index = "a")

circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)

circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y, panel.fun = function(x, y) {
    grey = c("#FFFFFF", "#CCCCCC", "#999999")
    i = get.cell.meta.data("sector.numeric.index")
    circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
    circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
    circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
})

circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = runif(100), y = runif(100))

circos.trackPlotRegion(factors = a$factor, y = a$y)
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")

circos.link("a", 0, "b", 0, top.ratio = 0.9)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red", border = "blue", top.ratio = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)

circos.clear()


###################################################
### code chunk number 2: figexample
###################################################
library(circlize)
set.seed(12345)
n = 10000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))
for(le in levels(a$factor)) {
    a$x[a$factor == le] = a$x[a$factor == le] * runif(1)
}

par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1, points.overflow.warning = FALSE)
circos.initialize(factors = a$factor, x = a$x)

bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
col = rep(c("#FF000010", "#00FF0010"), 4)
circos.trackPlotRegion(factors = a$factor, y = a$y, panel.fun = function(x, y) {
    circos.axis()
})
circos.trackPoints(a$factor, a$x, a$y, col = col, pch = 16, cex = 0.5)
circos.text(-1,0.5, "left", sector.index = "a")
circos.text(1,0.5, "right", sector.index = "a")

circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)

circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y, panel.fun = function(x, y) {
    grey = c("#FFFFFF", "#CCCCCC", "#999999")
    i = get.cell.meta.data("sector.numeric.index")
    circos.updatePlotRegion(bg.col = grey[i %% 3 + 1])
    circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
    circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
})

circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = runif(100), y = runif(100))

circos.trackPlotRegion(factors = a$factor, y = a$y)
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")

circos.link("a", 0, "b", 0, top.ratio = 0.9)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red", border = "blue", top.ratio = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)

circos.clear()


###################################################
### code chunk number 3: figtransformation
###################################################
library(circlize)
layout(cbind(c(1, 0, 2, 0, 3)), height = c(2,0.5,2, 0.5, 4))
par(mar = c(2, 2, 2, 2))
x = 1:10
y = rnorm(10)
plot(x, y, type = "l", axes = FALSE, ann = FALSE)
text(2, 0, "text", cex = 2)
rect(5, -1, 7, 1)
box()
axis(side = 1)

par(mar = c(1, 1, 1, 1))
factors = letters[1:3]
circos.par("canvas.xlim" = c(-sqrt(3)/2, sqrt(3)/2), "canvas.ylim" = c(1/2*0.6, 1), start.degree = 30, "track.margin" = c(0, 0), "gap.degree" = 0, "clock.wise" = FALSE, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "a", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.axis(h = "bottom")
circos.clear()

par(xpd = NA)
arrows(0, 1.33, 0, 1.07, code = 2)

par(mar = c(3, 3, 3, 3))
factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4)
circos.updatePlotRegion(sector.index = "c", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "c", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.axis(h = "bottom")
circos.clear()
box()
axis(side = 1)
axis(side = 2)
arrows(0, 1.5, 0, 1.07, code = 2)


###################################################
### code chunk number 4: figtransformation
###################################################
library(circlize)
layout(cbind(c(1, 0, 2, 0, 3)), height = c(2,0.5,2, 0.5, 4))
par(mar = c(2, 2, 2, 2))
x = 1:10
y = rnorm(10)
plot(x, y, type = "l", axes = FALSE, ann = FALSE)
text(2, 0, "text", cex = 2)
rect(5, -1, 7, 1)
box()
axis(side = 1)

par(mar = c(1, 1, 1, 1))
factors = letters[1:3]
circos.par("canvas.xlim" = c(-sqrt(3)/2, sqrt(3)/2), "canvas.ylim" = c(1/2*0.6, 1), start.degree = 30, "track.margin" = c(0, 0), "gap.degree" = 0, "clock.wise" = FALSE, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4, bg.border = NA)
circos.updatePlotRegion(sector.index = "a", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "a", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.axis(h = "bottom")
circos.clear()

par(xpd = NA)
arrows(0, 1.33, 0, 1.07, code = 2)

par(mar = c(3, 3, 3, 3))
factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(1, 10))
circos.trackPlotRegion(factors = factors, ylim = range(y), track.height = 0.4)
circos.updatePlotRegion(sector.index = "c", track.index = 1, bg.border = "black")
circos.lines(x, y, sector.index = "c", track.index = 1, straight = TRUE)
circos.text(2, 0, "text", cex = 2)
circos.rect(5, -1, 7, 1)
circos.axis(h = "bottom")
circos.clear()
box()
axis(side = 1)
axis(side = 2)
arrows(0, 1.5, 0, 1.07, code = 2)


###################################################
### code chunk number 5: figcoordinate
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = factor(letters[1:10], levels = sample(letters[1:10], 10))
circos.par("cell.padding" = c(0, 0, 0, 0), points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
for(l in letters[1:10]) {
    circos.rect(0,0,10,10,sector.index = l, track.index = 2, col = "#FF000040")
}

for(l in 1:4) {
    circos.rect(0,0,10,10,sector.index = "a", track.index = l, col = "#0000FF40")
}
show.index()
circos.clear()



###################################################
### code chunk number 6: figcoordinate
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = factor(letters[1:10], levels = sample(letters[1:10], 10))
circos.par("cell.padding" = c(0, 0, 0, 0), points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
for(l in letters[1:10]) {
    circos.rect(0,0,10,10,sector.index = l, track.index = 2, col = "#FF000040")
}

for(l in 1:4) {
    circos.rect(0,0,10,10,sector.index = "a", track.index = l, col = "#0000FF40")
}
show.index()
circos.clear()



###################################################
### code chunk number 7: figregion
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1), "xaxs" = "i", "yaxs" = "i")
factors = letters[1:8]
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 3, "start.degree" = 20, "track.margin" = c(0.05, 0.05), "clock.wise" = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))

circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.3, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/6, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/6, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})

x = seq(0, 1, length = 1000)
y = sqrt(1^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.8, length = 1000)
y = sqrt(0.8^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.4, length = 1000)
y = sqrt(0.4^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.2, length = 1000)
y = sqrt(0.2^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

draw.sector(center = c(0, 0), start = 20, end = 23, rou1 = 1, rou2 = 0.2, col = "#4DAF4A")
draw.sector(center = c(0, 0), start = 65, end = 68, rou1 = 1, rou2 = 0.2, col = "#4DAF4A")

circos.text(5, 5, "plotting region", sector.index = "a", track.index = 2)
circos.text(5, 10.5, "cell.padding[3]", sector.index = "a", track.index = 2)
circos.text(5, -0.5, "cell.padding[1]", sector.index = "a", track.index = 2)
circos.text(-0.5, 5, "cell.padding[2]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(10.5, 5, "cell.padding[4]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(5, -2, "track.margin[1]", sector.index = "a", track.index = 2)
circos.text(5, 12, "track.margin[2]", sector.index = "a", track.index = 2)
circos.text(-1.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(11.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.clear()


###################################################
### code chunk number 8: figregion
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1), "xaxs" = "i", "yaxs" = "i")
factors = letters[1:8]
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 3, "start.degree" = 20, "track.margin" = c(0.05, 0.05), "clock.wise" = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))

circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.3, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/6, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/6, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, bg.col = "#E41A1C", panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    cell.xlim = get.cell.meta.data("cell.xlim")
    cell.ylim = get.cell.meta.data("cell.ylim")
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "#377EB8", border = "black", lwd = 2)
    circos.rect(cell.xlim[1], cell.ylim[2], cell.xlim[2], cell.ylim[2]+(cell.ylim[2]-cell.ylim[1])/2, col = "#984EA3", border = NA)
    circos.rect(cell.xlim[1], cell.ylim[1]-(cell.ylim[2]-cell.ylim[1])/2, cell.xlim[2], cell.ylim[1], col = "#984EA3", border = NA)
    circos.lines(0:10, runif(11)*10)
    circos.rect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], lwd = 2, lty = 2)
})

x = seq(0, 1, length = 1000)
y = sqrt(1^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.8, length = 1000)
y = sqrt(0.8^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.4, length = 1000)
y = sqrt(0.4^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

x = seq(0, 0.2, length = 1000)
y = sqrt(0.2^2 - x^2)
lines(x, y, lty = 3, lwd = 2)

draw.sector(center = c(0, 0), start = 20, end = 23, rou1 = 1, rou2 = 0.2, col = "#4DAF4A")
draw.sector(center = c(0, 0), start = 65, end = 68, rou1 = 1, rou2 = 0.2, col = "#4DAF4A")

circos.text(5, 5, "plotting region", sector.index = "a", track.index = 2)
circos.text(5, 10.5, "cell.padding[3]", sector.index = "a", track.index = 2)
circos.text(5, -0.5, "cell.padding[1]", sector.index = "a", track.index = 2)
circos.text(-0.5, 5, "cell.padding[2]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(10.5, 5, "cell.padding[4]", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(5, -2, "track.margin[1]", sector.index = "a", track.index = 2)
circos.text(5, 12, "track.margin[2]", sector.index = "a", track.index = 2)
circos.text(-1.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.text(11.5, 5, "gap.degree", direction = "vertical_right", sector.index = "a", track.index = 2)
circos.clear()


###################################################
### code chunk number 9: figdirection
###################################################
par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par("track.margin" = c(0.1, 0.1), points.overflow.warning = FALSE)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])

text(0, 0, 'circos.par("clock.wise" = FALSE)', cex = 0.6)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("track.margin" = c(0.1, 0.1), clock.wise = TRUE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])


x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])
text(0, 0, 'circos.par("clock.wise" = TRUE)', cex = 0.6)


###################################################
### code chunk number 10: figdirection
###################################################
par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.par("track.margin" = c(0.1, 0.1), points.overflow.warning = FALSE)
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])

text(0, 0, 'circos.par("clock.wise" = FALSE)', cex = 0.6)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("track.margin" = c(0.1, 0.1), clock.wise = TRUE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.4, panel.fun = function(x, y) {
    circos.text(5, 5, get.cell.meta.data("sector.index"))
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.lines(xlim, c(0, 0))
    circos.lines(c(9, 10), c(0.5, 0))
    circos.lines(c(9, 10), c(-0.5, 0))
    circos.lines(c(0, 0), xlim)
    circos.lines(c(0.5, 0), c(9, 10))
    circos.lines(c(-0.5, 0), c(9, 10))
})
circos.clear()

x = seq(-0.7, 0.7, length = 100)
d = cbind(x, sqrt(1 - x^2))
lines(d)
arrows(d[99,1], d[99,2], d[100,1], d[100,2])


x = seq(-0.7, 0.7, length = 100)
d = cbind(x, -sqrt(1 - x^2))
lines(d)
arrows(d[2,1], d[2,2], d[1,1], d[1,2])
text(0, 0, 'circos.par("clock.wise" = TRUE)', cex = 0.6)


###################################################
### code chunk number 11: figlines
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:7]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5)
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
circos.text(5, 9, "type = 'l'", sector.index = "a")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
circos.text(5, 9, "type = 'o'", sector.index = "b")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
circos.text(5, 9, "type = 'h'", sector.index = "c")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "s")
circos.text(5, 9, "type = 's'", sector.index = "d")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", area = TRUE)
circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "e")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f", type = "o", area = TRUE)
circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "f")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g", type = "s", area = TRUE)
circos.text(5, 9, "type = 's', area = TRUE", sector.index = "g")
circos.clear()


###################################################
### code chunk number 12: figlines
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:7]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5)
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
circos.text(5, 9, "type = 'l'", sector.index = "a")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
circos.text(5, 9, "type = 'o'", sector.index = "b")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
circos.text(5, 9, "type = 'h'", sector.index = "c")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "s")
circos.text(5, 9, "type = 's'", sector.index = "d")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", area = TRUE)
circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "e")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f", type = "o", area = TRUE)
circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "f")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g", type = "s", area = TRUE)
circos.text(5, 9, "type = 's', area = TRUE", sector.index = "g")
circos.clear()


###################################################
### code chunk number 13: figtext
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
    circos.text(5, 9, "default_default", direction = "default")
    circos.text(0, 5, "vertical_left", direction = "vertical_left")
    circos.text(10, 5, "vertical_right", direction = "vertical_right")
    circos.text(5, 5, "horizontal", direction = "horizontal")
    circos.text(5, 1, "arc_arc_arc_arc_arc", direction = "arc")
})
circos.clear()


###################################################
### code chunk number 14: figtext
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5, panel.fun = function(x, y) {
    circos.text(5, 9, "default_default", direction = "default")
    circos.text(0, 5, "vertical_left", direction = "vertical_left")
    circos.text(10, 5, "vertical_right", direction = "vertical_right")
    circos.text(5, 5, "horizontal", direction = "horizontal")
    circos.text(5, 1, "arc_arc_arc_arc_arc", direction = "arc")
})
circos.clear()


###################################################
### code chunk number 15: figaxis
###################################################
library(circlize)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, panel.fun = function(x, y) {
    circos.text(5, 10, get.cell.meta.data("sector.index"))
})

circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.axis(sector.index = "a")
circos.axis(sector.index = "b", direction = "inside")
circos.axis(sector.index = "c", h = "bottom")
circos.axis(sector.index = "d", h = "bottom", direction = "inside")
circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick = FALSE)
circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick.percentage = 0.3, labels.away.percentage = 0.2, minor.ticks = 2, labels.direction = "vertical_right")
circos.clear()


###################################################
### code chunk number 16: figaxis
###################################################
library(circlize)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1, bg.border = NA, panel.fun = function(x, y) {
    circos.text(5, 10, get.cell.meta.data("sector.index"))
})

circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.axis(sector.index = "a")
circos.axis(sector.index = "b", direction = "inside")
circos.axis(sector.index = "c", h = "bottom")
circos.axis(sector.index = "d", h = "bottom", direction = "inside")
circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick = FALSE)
circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9), labels = c("a", "c", "e", "g", "f"), major.tick.percentage = 0.3, labels.away.percentage = 0.2, minor.ticks = 2, labels.direction = "vertical_right")
circos.clear()


###################################################
### code chunk number 17: figlink
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))

circos.clear()


###################################################
### code chunk number 18: figlink
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))

circos.clear()


###################################################
### code chunk number 19: figquadratic
###################################################
degree.minus = function(to, from, min.zero = TRUE) {
    if(min.zero) {
        return((to - from) %% 360)
    } else {
        if((to - from) %% 360 == 0) {
            return(360)
        } else {
            return((to - from) %% 360)
        }
    }
}
rotate.parabola = function(theta1, theta2, rou1, rou2 = rou1, theta = (theta1+theta2)/2, 
    rou = rou1 * abs(cos(degree.minus(theta1, theta2)/2/180*pi))*rou.ratio, rou.ratio = 0.5,
    n = 1001) {
    
    while(theta2 < theta1) {
        theta2 = theta2 + 360
    }
    
    delta_theta = degree.minus(theta2, theta1)
    
    flag = 0
    if(delta_theta > 180) {
        theta = theta + 180
        flag = 1
    }
    
    # y^2 = kx, y = +-sqrt(kx)
    b = rou1 * abs(sin(degree.minus(theta2, theta1)/2/180*pi))
    a = rou1 * abs(cos(degree.minus(theta2, theta1)/2/180*pi)) - rou
    k = b^2/a
    
    if(n %% 2 == 0) {
        n = n + 1
    }
    n.half = (n - 1) / 2
    x = numeric(n)
    y = numeric(n)
    x = c(n.half:1/n.half, 0, 1:n.half/n.half)*a
    y[1:n.half] = sqrt(k*x[1:n.half])
    y[n.half + 1] = 0
    y[1:n.half + n.half + 1] = -sqrt(k*x[1:n.half + n.half + 1])
    
    alpha = numeric(n)
    
    alpha[1:n.half] = atan(y[1:n.half]/x[1:n.half])*180/pi
    alpha[1:n.half + n.half + 1] = atan(y[1:n.half + n.half + 1]/x[1:n.half + n.half + 1])*180/pi
    alpha[n.half + 1] = 90
    
    d = sqrt(x^2 + y^2)
    x = d*cos((alpha + theta)/180*pi)
    y = d*sin((alpha + theta)/180*pi)
    
    center.x = rou*cos(theta/180*pi)
    center.y = rou*sin(theta/180*pi)
    
    x = x + center.x
    y = y + center.y
    
    if(!flag) {
        x = rev(x)
        y = rev(y)
    }

    return(cbind(x, y))
}

polar2Cartesian = function(d) {
    theta = d[, 1]/360 * 2 *pi
    rou = d[, 2]
    x = rou * cos(theta)
    y = rou * sin(theta)
    return(cbind(x, y))
}
par(mar = c(1, 1, 1, 1))
plot(c(-1, 1), c(-1, 1), axes = FALSE, ann = FALSE ,type = "n")
draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360, rou1 = 1, col = "white", border = "black")
d= rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 1, rou.ratio = 0.5)
lines(rbind(d, d[1, ]))
lines(c(cos(300/180*pi), cos(120/180*pi)), c(sin(300/180*pi), sin(120/180*pi)))
points(0, 0, pch = 16)
lines(c(0, sqrt(3)/4)+0.01, c(0, -3/4)+0.01, lwd = 4, col = "red")
lines(c(0, sqrt(3)/4/2)-0.01, c(0, -3/4/2)-0.01, lwd = 4, col = "blue")


###################################################
### code chunk number 20: figquadratic
###################################################
degree.minus = function(to, from, min.zero = TRUE) {
    if(min.zero) {
        return((to - from) %% 360)
    } else {
        if((to - from) %% 360 == 0) {
            return(360)
        } else {
            return((to - from) %% 360)
        }
    }
}
rotate.parabola = function(theta1, theta2, rou1, rou2 = rou1, theta = (theta1+theta2)/2, 
    rou = rou1 * abs(cos(degree.minus(theta1, theta2)/2/180*pi))*rou.ratio, rou.ratio = 0.5,
    n = 1001) {
    
    while(theta2 < theta1) {
        theta2 = theta2 + 360
    }
    
    delta_theta = degree.minus(theta2, theta1)
    
    flag = 0
    if(delta_theta > 180) {
        theta = theta + 180
        flag = 1
    }
    
    # y^2 = kx, y = +-sqrt(kx)
    b = rou1 * abs(sin(degree.minus(theta2, theta1)/2/180*pi))
    a = rou1 * abs(cos(degree.minus(theta2, theta1)/2/180*pi)) - rou
    k = b^2/a
    
    if(n %% 2 == 0) {
        n = n + 1
    }
    n.half = (n - 1) / 2
    x = numeric(n)
    y = numeric(n)
    x = c(n.half:1/n.half, 0, 1:n.half/n.half)*a
    y[1:n.half] = sqrt(k*x[1:n.half])
    y[n.half + 1] = 0
    y[1:n.half + n.half + 1] = -sqrt(k*x[1:n.half + n.half + 1])
    
    alpha = numeric(n)
    
    alpha[1:n.half] = atan(y[1:n.half]/x[1:n.half])*180/pi
    alpha[1:n.half + n.half + 1] = atan(y[1:n.half + n.half + 1]/x[1:n.half + n.half + 1])*180/pi
    alpha[n.half + 1] = 90
    
    d = sqrt(x^2 + y^2)
    x = d*cos((alpha + theta)/180*pi)
    y = d*sin((alpha + theta)/180*pi)
    
    center.x = rou*cos(theta/180*pi)
    center.y = rou*sin(theta/180*pi)
    
    x = x + center.x
    y = y + center.y
    
    if(!flag) {
        x = rev(x)
        y = rev(y)
    }

    return(cbind(x, y))
}

polar2Cartesian = function(d) {
    theta = d[, 1]/360 * 2 *pi
    rou = d[, 2]
    x = rou * cos(theta)
    y = rou * sin(theta)
    return(cbind(x, y))
}
par(mar = c(1, 1, 1, 1))
plot(c(-1, 1), c(-1, 1), axes = FALSE, ann = FALSE ,type = "n")
draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360, rou1 = 1, col = "white", border = "black")
d= rotate.parabola(theta1 = 270, theta2 = 330, rou1 = 1, rou.ratio = 0.5)
lines(rbind(d, d[1, ]))
lines(c(cos(300/180*pi), cos(120/180*pi)), c(sin(300/180*pi), sin(120/180*pi)))
points(0, 0, pch = 16)
lines(c(0, sqrt(3)/4)+0.01, c(0, -3/4)+0.01, lwd = 4, col = "red")
lines(c(0, sqrt(3)/4/2)-0.01, c(0, -3/4/2)-0.01, lwd = 4, col = "blue")


###################################################
### code chunk number 21: fighist
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")

circos.clear()



###################################################
### code chunk number 22: fighist
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")
circos.trackHist(factors = factors, x = x, draw.density = TRUE, force.ylim = FALSE, track.height = 0.1, col = "#CCCCCC", border = "#CCCCCC")

circos.clear()



###################################################
### code chunk number 23: sectorhighlight
###################################################
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5, col = "red", border = "black")
draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3, col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2, col = "green", border = "black")
draw.sector(c(0, 0), start = 120, end = 200, rou1 = 0.9, col = "#FF000040", border = "black")


###################################################
### code chunk number 24: sectorhighlight
###################################################
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5, col = "red", border = "black")
draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3, col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2, col = "green", border = "black")
draw.sector(c(0, 0), start = 120, end = 200, rou1 = 0.9, col = "#FF000040", border = "black")


###################################################
### code chunk number 25: figpart
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1),"clock.wise" = FALSE, "gap.degree" = 0, points.overflow.warning = FALSE)
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.points(runif(100), runif(100), pch = 16, cex = 0.5)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.lines(1:100/100, runif(100), pch = 16, cex = 0.5)
circos.clear()


###################################################
### code chunk number 26: figpart
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
circos.par("canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1),"clock.wise" = FALSE, "gap.degree" = 0, points.overflow.warning = FALSE)
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.points(runif(100), runif(100), pch = 16, cex = 0.5)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA)
circos.updatePlotRegion(sector.index = "a", bg.border = "black")
circos.lines(1:100/100, runif(100), pch = 16, cex = 0.5)
circos.clear()


###################################################
### code chunk number 27: fignested
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.text(0.5, 0.5, "outer circos")
})
circos.clear()

par(new = TRUE)
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2))
factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.text(0.5, 0.5, "inner circos")
})
circos.clear()


###################################################
### code chunk number 28: fignested
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.text(0.5, 0.5, "outer circos")
})
circos.clear()

par(new = TRUE)
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2))
factors = letters[1:3]
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.text(0.5, 0.5, "inner circos")
})
circos.clear()


###################################################
### code chunk number 29: figseperated
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par("canvas.xlim" = c(-1, 1.2), "canvas.ylim" = c(-1, 1.2), start.degree = -45, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a")
circos.text(0.5, 0.5, "first one")
circos.updatePlotRegion(sector.index = "b")
circos.text(0.5, 0.5, "first one")

circos.clear()

par(new = TRUE)
circos.par("canvas.xlim" = c(-1.2, 1), "canvas.ylim" = c(-1.2, 1), start.degree = -45)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "d")
circos.text(0.5, 0.5, "second one")
circos.updatePlotRegion(sector.index = "c")
circos.text(0.5, 0.5, "second one")

circos.clear()


###################################################
### code chunk number 30: figseperated
###################################################
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par("canvas.xlim" = c(-1, 1.2), "canvas.ylim" = c(-1, 1.2), start.degree = -45, points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "a")
circos.text(0.5, 0.5, "first one")
circos.updatePlotRegion(sector.index = "b")
circos.text(0.5, 0.5, "first one")

circos.clear()

par(new = TRUE)
circos.par("canvas.xlim" = c(-1.2, 1), "canvas.ylim" = c(-1.2, 1), start.degree = -45)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), bg.col = NA, bg.border = NA)
circos.updatePlotRegion(sector.index = "d")
circos.text(0.5, 0.5, "second one")
circos.updatePlotRegion(sector.index = "c")
circos.text(0.5, 0.5, "second one")

circos.clear()


###################################################
### code chunk number 31: figoutside
###################################################
library(circlize)
set.seed(12345)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par("canvas.xlim" = c(-1.5, 1.5), "canvas.ylim" = c(-1.5, 1.5), "gap.degree" = 10)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})
circos.lines(c(1/20, 0.5), c(1/20, 3), sector.index = "d", straight = TRUE)
circos.text(0.5, 3, "mark", sector.index = "d", adj = c(0.5, 0))

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})
text(0, 0, "this is\nthe center", cex = 1.5)
legend("bottomleft", pch = 1, legend = "this is the legend")
circos.clear()


###################################################
### code chunk number 32: figoutside
###################################################
library(circlize)
set.seed(12345)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par("canvas.xlim" = c(-1.5, 1.5), "canvas.ylim" = c(-1.5, 1.5), "gap.degree" = 10)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})
circos.lines(c(1/20, 0.5), c(1/20, 3), sector.index = "d", straight = TRUE)
circos.text(0.5, 3, "mark", sector.index = "d", adj = c(0.5, 0))

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(1:20/20, 1:20/20)
})
text(0, 0, "this is\nthe center", cex = 1.5)
legend("bottomleft", pch = 1, legend = "this is the legend")
circos.clear()


