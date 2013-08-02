### R code from vignette source 'draw-relations.Rnw'

###################################################
### code chunk number 1: draw-relations.Rnw:33-38 (eval = FALSE)
###################################################
## set.seed(12345)
## n = 9
## m = matrix(rnorm(n^2), n, n)
## colnames(m) = letters[1:n]
## m2 = cor(m)


###################################################
### code chunk number 2: draw-relations.Rnw:43-44 (eval = FALSE)
###################################################
## xlim = cbind(rep(0, n), apply(m2, 2, function(x) sum(abs(x)) - 1))


###################################################
### code chunk number 3: draw-relations.Rnw:53-68 (eval = FALSE)
###################################################
## library(circlize)
## factors = rownames(m2)
## colors = 1:n
## 
## par(mar = c(1, 1, 1, 1))
## circos.initialize(factors = factors, xlim = xlim)
## circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
##     panel.fun = function(x, y) {
##         xlim = get.cell.meta.data("xlim")
##         current.sector.index = get.cell.meta.data("sector.index")
##         circos.text(mean(xlim), 0.75, labels = current.sector.index,
##             direction = "horizontal")
##         i = get.cell.meta.data("sector.numeric.index")
##         circos.rect(min(xlim), 0, max(xlim), 0.25, col = colors[i])
##     })


###################################################
### code chunk number 4: draw-relations.Rnw:74-92 (eval = FALSE)
###################################################
## rn = rownames(m2)
## sector.sum = numeric(length(rn))
## for(i in 2:n) {
##     for(j in 1:(i-1)) {
##         sector.index1 = rn[i]
##         sector.index2 = rn[j]
##         circos.link(sector.index1,
##             c(sector.sum[i],sector.sum[i] + abs(m2[i, j])), 
##             sector.index2,
##             c(sector.sum[j], sector.sum[j] + abs(m2[i, j])),
##             col = ifelse(m2[i, j] > 0, "#FF0000A0", "#00FF00A0"),
##             border = "grey")
##         sector.sum[i] = sector.sum[i] + abs(m2[i, j])
##         sector.sum[j] = sector.sum[j] + abs(m2[i, j])
##     }
## }
## 
## circos.clear()


###################################################
### code chunk number 5: figcorrelation
###################################################
source("src/relation-01-example.R")


