### R code from vignette source 'circular_view_of_tables.Rnw'

###################################################
### code chunk number 1: circular_view_of_tables.Rnw:37-46
###################################################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]

rn = rownames(mat)
cn = colnames(mat)

mat


###################################################
### code chunk number 2: circular_view_of_tables.Rnw:52-58 (eval = FALSE)
###################################################
## factors = c(letters[1:3], rev(LETTERS[1:6]))
## factors = factor(factors, levels = factors)
## 
## col_sum = apply(mat, 2, sum)
## row_sum = apply(mat, 1, sum)
## xlim = cbind(rep(0, 9), c(row_sum, rev(col_sum)))


###################################################
### code chunk number 3: circular_view_of_tables.Rnw:65-76 (eval = FALSE)
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
## })


###################################################
### code chunk number 4: circular_view_of_tables.Rnw:81-91 (eval = FALSE)
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
### code chunk number 6: figchordbasic
###################################################
source("src/chordDiagram-01-basic.R")


###################################################
### code chunk number 7: circular_view_of_tables.Rnw:128-129 (eval = FALSE)
###################################################
## chordDiagram(mat)


###################################################
### code chunk number 8: circular_view_of_tables.Rnw:141-144 (eval = FALSE)
###################################################
## circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
## chordDiagram(mat)
## circos.clear()


###################################################
### code chunk number 9: circular_view_of_tables.Rnw:149-152 (eval = FALSE)
###################################################
## circos.par(start.degree = 90)
## chordDiagram(mat)
## circos.clear()


###################################################
### code chunk number 10: circular_view_of_tables.Rnw:157-158 (eval = FALSE)
###################################################
## chordDiagram(mat, order = c("A", "B", "a", "C", "D", "b", "E", "F", "c"))


###################################################
### code chunk number 11: circular_view_of_tables.Rnw:168-171 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE)
## chordDiagram(mat, directional = TRUE, directionGridHeight = 0.06)
## chordDiagram(mat, directional = TRUE, fromRows = FALSE, directionGridHeight = 0.06)


###################################################
### code chunk number 12: figchordcolor
###################################################
source("src/chordDiagram-02-color.R")


###################################################
### code chunk number 13: circular_view_of_tables.Rnw:197-201 (eval = FALSE)
###################################################
## grid.col = NULL
## grid.col[letters[1:3]] = c("red", "green", "blue")
## grid.col[LETTERS[1:6]] = "grey"
## chordDiagram(mat, grid.col = grid.col)


###################################################
### code chunk number 14: circular_view_of_tables.Rnw:207-208 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, transparency = 0.5)


###################################################
### code chunk number 15: circular_view_of_tables.Rnw:215-221 (eval = FALSE)
###################################################
## rand_color = function(n, alpha = 1) {
##     return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
## }
## col_mat = rand_color(length(mat), alpha = 0.5)
## dim(col_mat) = dim(mat)
## chordDiagram(mat, grid.col = grid.col, col = col_mat)


###################################################
### code chunk number 16: circular_view_of_tables.Rnw:228-231 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col,
##     col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)), rev(heat.colors(11))),
##     transparency = 0.5)


###################################################
### code chunk number 17: circular_view_of_tables.Rnw:238-240 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
## chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)


###################################################
### code chunk number 18: circular_view_of_tables.Rnw:245-246 (eval = FALSE)
###################################################
## chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))


###################################################
### code chunk number 19: figchordadvanced
###################################################
source("src/chordDiagram-03-advanced.R")


###################################################
### code chunk number 20: circular_view_of_tables.Rnw:314-320 (eval = FALSE)
###################################################
## list(ylim = c(0, 1),
##     track.height = circos.par("default.track.height"),
##     bg.col = NA,
##     bg.border = NA,
##     bg.lty = par("lty"),
##     bg.lwd = par("lwd"))


###################################################
### code chunk number 21: circular_view_of_tables.Rnw:326-328 (eval = FALSE)
###################################################
## chordDiagram(mat, annotationTrack = NULL,
##     preAllocateTracks = list(track.height = 0.3))


###################################################
### code chunk number 22: circular_view_of_tables.Rnw:334-337 (eval = FALSE)
###################################################
## chordDiagram(mat, annotationTrack = NULL,
##     preAllocateTracks = list(list(track.height = 0.1),
##                              list(bg.border = "black")))


###################################################
### code chunk number 23: circular_view_of_tables.Rnw:345-360 (eval = FALSE)
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
### code chunk number 24: figchordother
###################################################
source("src/chordDiagram-04-other.R")


###################################################
### code chunk number 25: circular_view_of_tables.Rnw:385-390
###################################################
set.seed(123)
mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]
mat


###################################################
### code chunk number 26: circular_view_of_tables.Rnw:393-394 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE, row.col = 1:5, transparency = 0.5)


###################################################
### code chunk number 27: circular_view_of_tables.Rnw:401-403 (eval = FALSE)
###################################################
## chordDiagram(cor(mat), symmetric = TRUE,
##     col = colorRamp2(c(-1, 0, 1), c("green", "white", "red"), transparency = 0.5))


###################################################
### code chunk number 28: circular_view_of_tables.Rnw:408-412
###################################################
for(cn in intersect(rownames(mat), colnames(mat))) {
	mat[cn, cn] = 0
}
mat


###################################################
### code chunk number 29: circular_view_of_tables.Rnw:415-416 (eval = FALSE)
###################################################
## chordDiagram(mat, directional = TRUE, row.col = 1:5, transparency = 0.5)


###################################################
### code chunk number 30: circular_view_of_tables.Rnw:429-438 (eval = FALSE)
###################################################
## mat1 = matrix(sample(20, 25, replace = TRUE), 5)
## 
## gap.degree = c(rep(2, 4), 10, rep(2, 4), 10)
## circos.par(gap.degree = gap.degree, start.degree = -5)
## chordDiagram(mat1, directional = TRUE, grid.col = rep(1:5, 2), transparency = 0.5)
## for(si in get.all.sector.index()) {
##     circos.axis(labels.cex = 0.5, sector.index = si, track.index = 2)
## }
## circos.clear()


###################################################
### code chunk number 31: circular_view_of_tables.Rnw:443-444 (eval = FALSE)
###################################################
## mat2 = mat1 / 2


###################################################
### code chunk number 32: circular_view_of_tables.Rnw:456-458 (eval = FALSE)
###################################################
## percent = sum(mat2) / sum(mat1)
## blank_degree = (360 - sum(gap.degree)) * (1 - percent)


###################################################
### code chunk number 33: circular_view_of_tables.Rnw:463-471 (eval = FALSE)
###################################################
## 
## gap.degree = c(rep(2, 4), blank_degree/2 + 10, rep(2, 4), blank_degree/2 + 10)
## circos.par(gap.degree = gap.degree, start.degree = -blank_degree/4 -5)
## chordDiagram(mat2, directional = TRUE, grid.col = rep(1:5, 2), transparency = 0.5)
## for(si in get.all.sector.index()) {
##     circos.axis(labels.cex = 0.5, sector.index = si, track.index = 2)
## }
## circos.clear()


###################################################
### code chunk number 34: figchordcompare
###################################################
source("src/chordDiagram-05-compare.R")


