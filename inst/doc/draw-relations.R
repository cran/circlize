### R code from vignette source 'draw-relations.Rnw'

###################################################
### code chunk number 1: draw-relations.Rnw:38-47
###################################################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]

rn = rownames(mat)
cn = colnames(mat)

mat


###################################################
### code chunk number 2: draw-relations.Rnw:53-59 (eval = FALSE)
###################################################
## factors = c(letters[1:3], LETTERS[1:6])
## factors = factor(factors, levels = factors)
## 
## col_sum = apply(mat, 2, sum)
## row_sum = apply(mat, 1, sum)
## xlim = cbind(rep(0, 9), c(row_sum, col_sum))


###################################################
### code chunk number 3: draw-relations.Rnw:66-90 (eval = FALSE)
###################################################
## par(mar = c(1, 1, 1, 1))
## circos.par(cell.padding = c(0, 0, 0, 0), 
##     gap.degree = c(2, 2, 10, 2, 2, 2, 2, 2, 10), start.degree = 5)
## circos.initialize(factors = factors, xlim = xlim)
## circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA, 
##     bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05, 
##     panel.fun = function(x, y) {
##         sector.name = get.cell.meta.data("sector.index")
##         xlim = get.cell.meta.data("xlim")
##         circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))
##         
##         # plot white border in the grids
##         if(sector.name %in% rn) {
##             for(i in seq_len(ncol(mat))) {
##                 circos.lines(rep(sum(mat[sector.name, seq_len(i)]), 2), c(0, 1), 
##                     col = "white")
##             }
##         } else if(sector.name %in% cn) {
##             for(i in rev(seq_len(nrow(mat)))) {
##                 circos.lines(rep(sum(mat[seq_len(i), sector.name]), 2), c(0, 1), 
##                    col = "white")
##             }
##         }
## })


###################################################
### code chunk number 4: draw-relations.Rnw:95-105 (eval = FALSE)
###################################################
## col = c("#FF000020", "#00FF0020", "#0000FF20")
## for(i in seq_len(nrow(mat))) {
##     for(j in seq_len(ncol(mat))) {
##         circos.link(rn[i], c(sum(mat[i, seq_len(j-1)]), sum(mat[i, seq_len(j)])),
##             cn[j], c(sum(mat[seq_len(i-1), j]), sum(mat[seq_len(i), j])), 
##             col = col[i], border = "white")
##     }
## }
## 
## circos.clear()


###################################################
### code chunk number 5: figtable
###################################################
source("src/relation-02-table.R")


###################################################
### code chunk number 6: draw-relations.Rnw:128-130 (eval = FALSE)
###################################################
## chordDiagram(mat)
## circos.clear()


###################################################
### code chunk number 7: draw-relations.Rnw:141-144 (eval = FALSE)
###################################################
## circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
## chordDiagram(mat)
## circos.clear()


###################################################
### code chunk number 8: draw-relations.Rnw:149-152 (eval = FALSE)
###################################################
## circos.par(start.degree = 90)
## chordDiagram(mat)
## circos.clear()


###################################################
### code chunk number 9: draw-relations.Rnw:157-158 (eval = FALSE)
###################################################
## chordDiagram(mat, order = c("A", "B", "a", "C", "D", "b", "E", "F", "c"))


###################################################
### code chunk number 10: draw-relations.Rnw:167-169 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE)
## chordDiagram(mat, directional = TRUE, directionGridHeight = 0.06)


###################################################
### code chunk number 11: figchordbasic
###################################################
source("src/chordDiagram-01-basic.R")


###################################################
### code chunk number 12: draw-relations.Rnw:189-193 (eval = FALSE)
###################################################
## grid.col = NULL
## grid.col[letters[1:3]] = c("red", "green", "blue")
## grid.col[LETTERS[1:6]] = "grey"
## chordDiagram(mat, grid.col = grid.col)


###################################################
### code chunk number 13: draw-relations.Rnw:199-200 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, transparency = 0.5)


###################################################
### code chunk number 14: draw-relations.Rnw:207-213 (eval = FALSE)
###################################################
## rand_color = function(n, alpha = 1) {
##     return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
## }
## col_mat = rand_color(length(mat), alpha = 0.5)
## dim(col_mat) = dim(mat)
## chordDiagram(mat, grid.col = grid.col, col = col_mat)


###################################################
### code chunk number 15: draw-relations.Rnw:220-223 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col,
##     col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)), rev(heat.colors(11))),
##     transparency = 0.5)


###################################################
### code chunk number 16: draw-relations.Rnw:230-232 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
## chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)


###################################################
### code chunk number 17: draw-relations.Rnw:237-238 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))


###################################################
### code chunk number 18: figchordcolor
###################################################
source("src/chordDiagram-02-color.R")


###################################################
### code chunk number 19: draw-relations.Rnw:302-308 (eval = FALSE)
###################################################
## list(ylim = c(0, 1),
##     track.height = circos.par("default.track.height"),
##     bg.col = NA,
##     bg.border = NA,
##     bg.lty = par("lty"),
##     bg.lwd = par("lwd"))


###################################################
### code chunk number 20: draw-relations.Rnw:314-316 (eval = FALSE)
###################################################
## chordDiagram(mat, annotationTrack = NULL,
##     preAllocateTracks = list(track.height = 0.3))


###################################################
### code chunk number 21: draw-relations.Rnw:322-325 (eval = FALSE)
###################################################
## chordDiagram(mat, annotationTrack = NULL,
##     preAllocateTracks = list(list(track.height = 0.1),
##                              list(bg.border = "black")))


###################################################
### code chunk number 22: draw-relations.Rnw:333-348 (eval = FALSE)
###################################################
## chordDiagram(mat, annotationTrack = "grid",
##     preAllocateTracks = list(track.height = 0.3))
## circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
##     xlim = get.cell.meta.data("xlim")
##     ylim = get.cell.meta.data("ylim")
##     sector.name = get.cell.meta.data("sector.index")
##     if(sector.name %in% rn) {
##         label = paste0(rep(sector.name, 5), collapse="")
##         circos.text(mean(xlim), ylim[1], label, facing = "bending", adj = c(0.5, 0))
##     }
##     if(sector.name %in% cn) {
##         label = paste0(rep(sector.name, 5), collapse="")
##         circos.text(mean(xlim), ylim[1], label, facing = "clockwise", adj = c(0, 0.5))
##     }
## }, bg.border = NA)


###################################################
### code chunk number 23: figchordadvanced
###################################################
source("src/chordDiagram-03-advanced.R")


###################################################
### code chunk number 24: draw-relations.Rnw:370-375
###################################################
set.seed(123)
mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]
mat


###################################################
### code chunk number 25: draw-relations.Rnw:378-380 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE, 
##     row.col = 1:5, transparency = 0.5)


###################################################
### code chunk number 26: draw-relations.Rnw:386-388 (eval = FALSE)
###################################################
## chordDiagram(cor(mat), symmetric = TRUE,
##     col = colorRamp2(c(-1, 0, 1), c("green", "white", "red"), transparency = 0.5))


###################################################
### code chunk number 27: draw-relations.Rnw:393-397
###################################################
for(cn in intersect(rownames(mat), colnames(mat))) {
	mat[cn, cn] = 0
}
mat


###################################################
### code chunk number 28: draw-relations.Rnw:400-402 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE, 
##     row.col = 1:5, transparency = 0.5)


###################################################
### code chunk number 29: figchordother
###################################################
source("src/chordDiagram-04-other.R")


