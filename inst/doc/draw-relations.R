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


###################################################
### code chunk number 6: draw-relations.Rnw:117-123 (eval = FALSE)
###################################################
## mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
## rownames(mat) = letters[1:3]
## colnames(mat) = LETTERS[1:6]
## 
## rn = rownames(mat)
## cn = colnames(mat)


###################################################
### code chunk number 7: draw-relations.Rnw:129-135 (eval = FALSE)
###################################################
## factors = c(letters[1:3], LETTERS[1:6])
## factors = factor(factors, levels = factors)
## 
## col_sum = apply(mat, 2, sum)
## row_sum = apply(mat, 1, sum)
## xlim = cbind(rep(0, 9), c(row_sum, col_sum))


###################################################
### code chunk number 8: draw-relations.Rnw:146-182 (eval = FALSE)
###################################################
## par(mar = c(1, 1, 1, 1))
## circos.par(cell.padding = c(0, 0, 0, 0), clock.wise = FALSE, 
##     gap.degree = c(2, 2, 10, 2, 2, 2, 2, 2, 10), start.degree = 5)
## circos.initialize(factors = factors, xlim = xlim, 
##     sector.width = c(row_sum/sum(row_sum), col_sum/sum(col_sum)))
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA, 
##     bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05, 
##     panel.fun = function(x, y) {
##         sector.name = get.cell.meta.data("sector.index")
##         xlim = get.cell.meta.data("xlim")
##         circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))
## 
##         if(sector.name %in% rn) {
##             for(i in seq_len(ncol(mat))) {
##                 circos.lines(rep(sum(mat[sector.name, seq_len(i)]), 2), c(0, 1), 
##                     col = "white")
##             }
##         } else if(sector.name %in% cn) {
##             for(i in seq_len(nrow(mat))) {
##                 circos.lines(rep(sum(mat[ seq_len(i), sector.name]), 2), c(0, 1), 
##                    col = "white")
##             }
##         }
##     })
## 
## 
## col = c("#FF000020", "#00FF0020", "#0000FF20")
## for(i in seq_len(nrow(mat))) {
##     for(j in seq_len(ncol(mat))) {
##         circos.link(rn[i], c(sum(mat[i, seq_len(j-1)]), sum(mat[i, seq_len(j)])),
##                     cn[j], c(sum(mat[seq_len(i-1), j]), sum(mat[seq_len(i), j])), 
##                     col = col[i], border = "white")
##     }
## }
## 
## circos.clear()


###################################################
### code chunk number 9: figtable
###################################################
source("src/relation-02-table.R")


