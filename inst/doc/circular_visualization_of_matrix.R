## ----echo = FALSE--------------------------------------------------------
library(knitr)
opts_chunk$set(fig.pos = "")

library(circlize)
chordDiagram = function(...) {
    circos.par(unit.circle.segments = 300)
    circlize::chordDiagram(...)
}

## ------------------------------------------------------------------------
library(circlize)

mat = matrix(1:18, 3, 6)
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)

mat

## ------------------------------------------------------------------------
rn = rownames(mat)
cn = colnames(mat)

factors = c(rn, cn)
factors = factor(factors, levels = factors)

col_sum = apply(mat, 2, sum)
row_sum = apply(mat, 1, sum)
xlim = cbind(rep(0, length(factors)), c(row_sum, col_sum))

## ----chord_diagram_by_hand_initialize, eval = FALSE----------------------
#  circos.par(cell.padding = c(0, 0, 0, 0))
#  circos.initialize(factors = factors, xlim = xlim)
#  circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA,
#      bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05,
#      panel.fun = function(x, y) {
#          sector.name = get.cell.meta.data("sector.index")
#          xlim = get.cell.meta.data("xlim")
#          circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))
#  })

## ----chord_diagram_by_hand_add_links, eval = FALSE-----------------------
#  col = c("#FF000020", "#00FF0020", "#0000FF20")
#  for(i in seq_len(nrow(mat))) {
#      for(j in seq_len(ncol(mat))) {
#          circos.link(rn[i], c(sum(mat[i, seq_len(j-1)]), sum(mat[i, seq_len(j)])),
#              cn[j], c(sum(mat[seq_len(i-1), j]), sum(mat[seq_len(i), j])),
#              col = col[i], border = "white")
#      }
#  }
#  
#  circos.clear()

## ----chord_diagram_by_hand, echo = FALSE, fig.align = "center", out.width = "0.6\\textwidth", fig.cap = "Matrix in circular layout."----
circos.par(points.overflow.warning = FALSE)
circos.par(cell.padding = c(0, 0, 0, 0))
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA, 
    bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05, 
    panel.fun = function(x, y) {
        sector.name = get.cell.meta.data("sector.index")
        xlim = get.cell.meta.data("xlim")
        circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))
})
col = c("#FF000020", "#00FF0020", "#0000FF20")
for(i in seq_len(nrow(mat))) {
    for(j in seq_len(ncol(mat))) {
        circos.link(rn[i], c(sum(mat[i, seq_len(j-1)]), sum(mat[i, seq_len(j)])),
            cn[j], c(sum(mat[seq_len(i-1), j]), sum(mat[seq_len(i), j])), 
            col = col[i], border = "white")
    }
}

circos.clear()

## ----chord_diagram_basic_simple, eval = FALSE----------------------------
#  set.seed(999)
#  chordDiagram(mat)
#  circos.clear()

## ----chord_diagram_basic_gap_degree, eval = FALSE------------------------
#  circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
#  chordDiagram(mat)
#  circos.clear()

## ----chord_diagram_basic_start_degree, eval = FALSE----------------------
#  circos.par(start.degree = 90)
#  chordDiagram(mat)
#  circos.clear()

## ----chord_diagram_basic_order, eval = FALSE-----------------------------
#  chordDiagram(mat, order = c("S1", "E1", "E2", "S2", "E3", "E4", "S3", "E5", "E6"))

## ----chord_diagram_basic, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Basic usages of {\\tt chordDiagram}. A) default style; B) set {\\tt gap.degree}; C) set {\\tt start.degree}; D) set orders of sectors."----
par(mfrow = c(2, 2))
set.seed(999)
chordDiagram(mat)
circos.clear()
text(-0.9, 0.9, "A", cex = 1.5)
circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat)
circos.clear()
text(-0.9, 0.9, "B", cex = 1.5)
circos.par(start.degree = 90)
chordDiagram(mat)
circos.clear()
text(-0.9, 0.9, "C", cex = 1.5)
chordDiagram(mat, order = c("S1", "E1", "E2", "S2", "E3", "E4", "S3", "E5", "E6"))
text(-0.9, 0.9, "D", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_color_grid, eval = FALSE------------------------------
#  grid.col = NULL  # just create the variable
#  grid.col[rn] = c("red", "green", "blue")
#  grid.col[cn] = "grey"
#  chordDiagram(mat, grid.col = grid.col)

## ----chord_diagram_color_transparency, eval = FALSE----------------------
#  chordDiagram(mat, grid.col = grid.col, transparency = 0)

## ----chord_diagram_color_mat, eval = FALSE-------------------------------
#  col_mat = rand_color(length(mat), transparency = 0.5)
#  dim(col_mat) = dim(mat)  # to make sure it is a matrix
#  chordDiagram(mat, grid.col = grid.col, col = col_mat)

## ----chord_diagram_color_fun, eval = FALSE-------------------------------
#  col_fun = colorRamp2(quantile(mat, seq(0, 1, length.out = 18)), rev(rainbow(18)))
#  chordDiagram(mat, grid.col = grid.col, col = col_fun)

## ----chord_diagram_color_row_col, eval = FALSE, echo = -2----------------
#  chordDiagram(mat, grid.col = grid.col, row.col = 1:3)
#  text(-0.9, 0.9, "E", cex = 1.5)
#  chordDiagram(mat, grid.col = grid.col, column.col = 1:6)

## ----chord_diagram_color, echo = FALSE, fig.align = "center", out.width = "0.6\\textheight", out.height = "0.9\\textheight", fig.width = 7, fig.height = 10.5, fig.cap = "Color settings in {\\tt chordDiagram}. A) set {\\tt grid.col}; B) set {\\tt transparency}; C) set {\\tt col} as a matrix; D) set {\\tt col} as a function; E) set {\\tt row.col}; F) set {\\tt column.col}."----
par(mfrow = c(3, 2))
grid.col = NULL  # just create the variable
grid.col[rn] = c("red", "green", "blue")
grid.col[cn] = "grey"
chordDiagram(mat, grid.col = grid.col)
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, transparency = 0)
text(-0.9, 0.9, "B", cex = 1.5)
col_mat = rand_color(length(mat), transparency = 0.5)
dim(col_mat) = dim(mat)  # to make sure it is a matrix
chordDiagram(mat, grid.col = grid.col, col = col_mat)
text(-0.9, 0.9, "C", cex = 1.5)
col_fun = colorRamp2(quantile(mat, seq(0, 1, length.out = 18)), rev(rainbow(18)))
chordDiagram(mat, grid.col = grid.col, col = col_fun)
text(-0.9, 0.9, "D", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, row.col = 1:3)
text(-0.9, 0.9, "E", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, column.col = 1:6)
text(-0.9, 0.9, "F", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_style_cross_1, eval = FALSE---------------------------
#  chordDiagram(mat, grid.col = grid.col, link.order = c(1, 1))
#  chordDiagram(mat, grid.col = grid.col, link.order = c(1, -1))
#  chordDiagram(mat, grid.col = grid.col, link.order = c(-1, 1))
#  chordDiagram(mat, grid.col = grid.col, link.order = c(-1, -1))

## ----chord_diagram_link_order, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Link orders in sectors. A) set {\\tt link.order} to {\\tt c(1, 1)}; B) set {\\tt link.order} to {\\tt c(1, -1)}; C) set {\\tt link.order} to {\\tt c(-1, 1)}; D) set {\\tt link.order} to {\\tt c(-1, -1)}."----
par(mfrow = c(2, 2))
chordDiagram(mat, grid.col = grid.col, link.order = c(1, 1))
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, link.order = c(1, -1))
text(-0.9, 0.9, "B", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, link.order = c(-1, 1))
text(-0.9, 0.9, "C", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, link.order = c(-1, -1))
text(-0.9, 0.9, "D", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_style_cross_2, eval = FALSE---------------------------
#  chordDiagram(mat, grid.col = grid.col, cross = TRUE)

## ----chord_diagram_style_scalar, eval = FALSE----------------------------
#  chordDiagram(mat, grid.col = grid.col, link.lwd = 2, link.lty = 2, link.border = "black")

## ----chord_diagram_style_fullmat, eval = FALSE---------------------------
#  lwd_mat = matrix(1, nrow = nrow(mat), ncol = ncol(mat))
#  rownames(lwd_mat) = rownames(mat)
#  colnames(lwd_mat) = colnames(mat)
#  lwd_mat[mat > 12] = 2
#  
#  border_mat = matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
#  rownames(border_mat) = rownames(mat)
#  colnames(border_mat) = colnames(mat)
#  border_mat[mat > 12] = "black"
#  
#  chordDiagram(mat, grid.col = grid.col, link.lwd = lwd_mat, link.border = border_mat)

## ----chord_diagram_style_submatrix, eval = FALSE-------------------------
#  border_mat2 = matrix("black", nrow = 1, ncol = ncol(mat))
#  rownames(border_mat2) = rownames(mat)[2]
#  colnames(border_mat2) = colnames(mat)
#  
#  chordDiagram(mat, grid.col = grid.col, link.lwd = 2, link.border = border_mat2)

## ----chord_diagram_style_dataframe, eval = FALSE-------------------------
#  lty_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(1, 2, 3))
#  lwd_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(2, 2, 2))
#  border_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(1, 1, 1))
#  chordDiagram(mat, grid.col = grid.col, link.lty = lty_df, link.lwd = lwd_df,
#      link.border = border_df)

## ----chord_diagram_style, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Link style settings in {\\tt chordDiagram}. A) graphic parameters set as scalar; B) graphic parameters set as matrix; C) graphic parameters set as sub matrix. D) graphic parameters set as a three-column data frame."----
par(mfrow = c(2, 2))
chordDiagram(mat, grid.col = grid.col, link.lwd = 2, link.lty = 2, link.border = "black")
text(-0.9, 0.9, "A", cex = 1.5)
lwd_mat = matrix(1, nrow = nrow(mat), ncol = ncol(mat))
rownames(lwd_mat) = rownames(mat)
colnames(lwd_mat) = colnames(mat)
lwd_mat[mat > 12] = 2

border_mat = matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
rownames(border_mat) = rownames(mat)
colnames(border_mat) = colnames(mat)
border_mat[mat > 12] = "black"

chordDiagram(mat, grid.col = grid.col, link.lwd = lwd_mat, link.border = border_mat)
text(-0.9, 0.9, "B", cex = 1.5)
border_mat2 = matrix("black", nrow = 1, ncol = ncol(mat))
rownames(border_mat2) = rownames(mat)[2]
colnames(border_mat2) = colnames(mat)

chordDiagram(mat, grid.col = grid.col, link.lwd = 2, link.border = border_mat2)
text(-0.9, 0.9, "C", cex = 1.5)
lty_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(1, 2, 3))
lwd_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(2, 2, 2))
border_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(1, 1, 1))
chordDiagram(mat, grid.col = grid.col, link.lty = lty_df, link.lwd = lwd_df,
    link.border = border_df)
text(-0.9, 0.9, "D", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_highlight_row, eval = FALSE---------------------------
#  chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))

## ----chord_diagram_highlight_mat, eval = FALSE---------------------------
#  col_mat[mat < 12] = "#00000000"
#  chordDiagram(mat, grid.col = grid.col, col = col_mat)

## ----chord_diagram_highlight_fun, eval = FALSE---------------------------
#  col_fun = function(x) ifelse(x < 12, "#00000000", "#FF000080")
#  chordDiagram(mat, grid.col = grid.col, col = col_fun)

## ----chord_diagram_highlight_df, eval = FALSE----------------------------
#  col_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"),
#      c("#FF000080", "#00FF0080", "#0000FF80"))
#  chordDiagram(mat, grid.col = grid.col, col = col_df)

## ----chord_diagram_highlight, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Highlight links by colors. A) set {\\tt row.col}; B) set by matrix; C) set by color function; D) set by a three-column data frame."----
par(mfrow = c(2, 2))
chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
text(-0.9, 0.9, "A", cex = 1.5)
col_mat[mat < 12] = "#00000000"
chordDiagram(mat, grid.col = grid.col, col = col_mat) 
text(-0.9, 0.9, "B", cex = 1.5)
col_fun = function(x) ifelse(x < 12, "#00000000", "#FF000080")
chordDiagram(mat, grid.col = grid.col, col = col_fun)
text(-0.9, 0.9, "C", cex = 1.5)
col_df = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), 
    c("#FF000080", "#00FF0080", "#0000FF80"))
chordDiagram(mat, grid.col = grid.col, col = col_df)
text(-0.9, 0.9, "D", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_directional_simple, eval = FALSE, echo = c(1, 3, 5)----
#  chordDiagram(mat, directional = TRUE)
#  text(-0.9, 0.9, "A", cex = 1.5)
#  chordDiagram(mat, directional = TRUE, diffHeight = 0.08)
#  text(-0.9, 0.9, "B", cex = 1.5)
#  chordDiagram(mat, directional = TRUE, fromRows = FALSE)
#  text(-0.9, 0.9, "C", cex = 1.5)

## ------------------------------------------------------------------------
mat2 = matrix(sample(100, 35), nrow = 5)
rownames(mat2) = letters[1:5]
colnames(mat2) = letters[1:7]
mat2

## ----chord_diagram_directional_overlap, eval = FALSE---------------------
#  chordDiagram(mat2, directional = TRUE, row.col = 1:5)

## ------------------------------------------------------------------------
mat3 = mat2
for(cn in intersect(rownames(mat3), colnames(mat3))) {
    mat3[cn, cn] = 0
}
mat3

## ----chord_diagram_directional_non_selfloop, eval = FALSE----------------
#  chordDiagram(mat3, directional = TRUE, row.col = 1:5)

## ----chord_diagram_directional_arrow, eval = FALSE-----------------------
#  arr.col = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"),
#      c("black", "black", "black"))
#  chordDiagram(mat, directional = TRUE, direction.type = "arrows",
#      link.arr.col = arr.col, link.arr.length = 0.2)

## ----chord_diagram_directional_arrow2, eval = FALSE----------------------
#  arr.col = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"),
#      c("black", "black", "black"))
#  chordDiagram(mat, directional = TRUE, direction.type = c("diffHeight", "arrows"),
#      link.arr.col = arr.col, link.arr.length = 0.2)

## ----chord_diagram_directional, echo = FALSE, fig.align = "center", out.width = "0.6\\textheight", out.height = "0.9\\textheight", fig.width = 7, fig.height = 10.5, fig.cap = "Visualization of directional matrix. A) with default settings; B) set difference of two feet of links; C) set the starting feet; D) row names and column names have overlaps; E, F) directions are represented by arrows."----
par(mfrow = c(3, 2))
chordDiagram(mat, directional = TRUE)
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, directional = TRUE, diffHeight = 0.08)
text(-0.9, 0.9, "B", cex = 1.5)
chordDiagram(mat, directional = TRUE, fromRows = FALSE)
text(-0.9, 0.9, "C", cex = 1.5)
chordDiagram(mat2, directional = TRUE, row.col = 1:5)
text(-0.9, 0.9, "D", cex = 1.5)
arr.col = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), 
    c("black", "black", "black"))
chordDiagram(mat, directional = TRUE, direction.type = "arrows",
    link.arr.col = arr.col, link.arr.length = 0.2)
text(-0.9, 0.9, "E", cex = 1.5)
arr.col = data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), 
    c("black", "black", "black"))
chordDiagram(mat, directional = TRUE, direction.type = c("diffHeight", "arrows"),
    link.arr.col = arr.col, link.arr.length = 0.2)
text(-0.9, 0.9, "F", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_symmetric_show, eval = FALSE--------------------------
#  mat3 = matrix(rnorm(100), 10)
#  colnames(mat3) = letters[1:10]
#  chordDiagram(cor(mat3), symmetric = TRUE,
#      col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))

## ----chord_diagram_symmetric_hidden, eval = FALSE, echo = FALSE----------
#  chordDiagram(cor(mat3), col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))

## ----chord_diagram_symmetric, echo = FALSE, fig.align = "center", out.width = "\\textwidth", out.height = "0.5\\textwidth", fig.width = 7, fig.height = 3.5, fig.cap = "Visualization of symmetric matrix. A) set {\\tt symmetric} to {\\tt TRUE}; B) set {\\tt symmetric} to {\\tt FALSE}."----
par(mfrow = c(1, 2))
mat3 = matrix(rnorm(100), 10)
colnames(mat3) = letters[1:10]
chordDiagram(cor(mat3), symmetric = TRUE,
    col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(cor(mat3), col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")))
text(-0.9, 0.9, "B", cex = 1.5)
par(mfrow = c(1, 1))

## ----echo = 2:3----------------------------------------------------------
pdf(NULL)
chordDiagram(mat)
circos.info()
invisible(dev.off())

## ----chord_diagram_default_track_simple, eval = FALSE, echo = c(1, 3, 4, 6)----
#  chordDiagram(mat, grid.col = grid.col, annotationTrack = "grid")
#  text(-0.9, 0.9, "A", cex = 1.5)
#  chordDiagram(mat, grid.col = grid.col, annotationTrack = c("name", "grid"),
#      annotationTrackHeight = c(0.03, 0.01))
#  text(-0.9, 0.9, "B", cex = 1.5)
#  chordDiagram(mat, annotationTrack = NULL)
#  text(-0.9, 0.9, "C", cex = 1.5)

## ----chord_diagram_default_track, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Track organization in {\\tt chordDiagram}. A) only show the grid track; B) set label track and grid track with heights; C) do not add label track or grid track."----
par(mfrow = c(2, 2))
chordDiagram(mat, grid.col = grid.col, annotationTrack = "grid")
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, grid.col = grid.col, annotationTrack = c("name", "grid"),
    annotationTrackHeight = c(0.03, 0.01))
text(-0.9, 0.9, "B", cex = 1.5)
chordDiagram(mat, annotationTrack = NULL)
text(-0.9, 0.9, "C", cex = 1.5)
par(mfrow = c(1, 1))

## ----echo = 2:3----------------------------------------------------------
pdf(NULL)
chordDiagram(mat, preAllocateTracks = 2)
circos.info()
invisible(dev.off())

## ----eval = FALSE--------------------------------------------------------
#  list(ylim = c(0, 1),
#       track.height = circos.par("track.height"),
#       bg.col = NA,
#       bg.border = NA,
#       bg.lty = par("lty"),
#       bg.lwd = par("lwd"))

## ----eval = FALSE--------------------------------------------------------
#  chordDiagram(mat, annotationTrack = NULL,
#      preAllocateTracks = list(track.height = 0.3))
#  circos.info(sector.index = "S1", track.index = 1)

## ----eval = FALSE--------------------------------------------------------
#  chordDiagram(mat, annotationTrack = NULL,
#      preAllocateTracks = list(list(track.height = 0.1),
#                               list(bg.border = "black")))

## ----chord_diagram_labels_show, eval = FALSE-----------------------------
#  chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3))
#  # we go back to the first track and customize sector labels
#  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#      xlim = get.cell.meta.data("xlim")
#      ylim = get.cell.meta.data("ylim")
#      sector.name = get.cell.meta.data("sector.index")
#      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
#          niceFacing = TRUE, adj = c(0, 0.5))
#  }, bg.border = NA)

## ----chord_diagram_labels_inside, eval = FALSE---------------------------
#  chordDiagram(mat, annotationTrack = "grid", annotationTrackHeight = 0.15)
#  for(si in get.all.sector.index()) {
#      xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
#      ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
#      circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1,
#          facing = "bending.inside", col = "white")
#  }

## ----chord_diagram_labels_multile_style, eval = FALSE--------------------
#  chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
#  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#      xlim = get.cell.meta.data("xlim")
#      xplot = get.cell.meta.data("xplot")
#      ylim = get.cell.meta.data("ylim")
#      sector.name = get.cell.meta.data("sector.index")
#  
#      if(abs(xplot[2] - xplot[1]) < 20) {
#          circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
#              niceFacing = TRUE, adj = c(0, 0.5))
#      } else {
#          circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
#              niceFacing = TRUE, adj = c(0.5, 0))
#      }
#  }, bg.border = NA)

## ----chord_diagram_labels, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Customize sector labels. A) put sector labels in radical direction; B) sector labels are put inside grids; C) sector labels are put in different direction according the width of sectors."----
par(mfrow = c(2, 2))
chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3))
# we go back to the first track and customize sector labels
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", 
        niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, annotationTrack = "grid", annotationTrackHeight = 0.15)
for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
        facing = "bending.inside", col = "white")
}
text(-0.9, 0.9, "B", cex = 1.5)
circos.par(points.overflow.warning = FALSE)
chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")

    if(abs(xplot[2] - xplot[1]) < 20) {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
            niceFacing = TRUE, adj = c(0, 0.5))
    } else {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
            niceFacing = TRUE, adj = c(0.5, 0))
    }
}, bg.border = NA)
circos.clear()
text(-0.9, 0.9, "C", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_axes_simple, eval = FALSE-----------------------------
#  chordDiagram(mat, grid.col = grid.col)
#  for(si in get.all.sector.index()) {
#      # here the index for the grid track is 2
#      circos.axis(h = "top", labels.cex = 0.3, major.tick.percentage = 0.2,
#      sector.index = si, track.index = 2)
#  }

## ----chord_diagram_axes_two, eval = FALSE--------------------------------
#  # similar as the previous example, but we only plot the grid track
#  chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
#  for(si in get.all.sector.index()) {
#      circos.axis(h = "top", labels.cex = 0.3, major.tick.percentage = 0.2,
#          sector.index = si, track.index = 2)
#  }
#  
#  # the second axis as well as the sector labels are added in this track
#  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#      xlim = get.cell.meta.data("xlim")
#      xplot = get.cell.meta.data("xplot")
#      ylim = get.cell.meta.data("ylim")
#      sector.name = get.cell.meta.data("sector.index")
#  
#      if(abs(xplot[2] - xplot[1]) > 20) {
#          circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3) # dotted line
#          for(p in seq(0.2, 1, by = 0.2)) {
#              circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim) + 0.1,
#                  p, cex = 0.3, adj = c(0.5, 0), niceFacing = TRUE)
#          }
#      }
#      circos.text(mean(xlim), 1, sector.name, niceFacing = TRUE, adj = c(0.5, 0))
#  }, bg.border = NA)
#  circos.clear()

## ----chord_diagram_axes, echo = FALSE, fig.align = "center", out.width = "\\textwidth", out.height = "0.5\\textwidth", fig.width = 7, fig.height = 3.5, fig.cap = "Customize sector axes. A) add axes to the grid track; B) add another percentage axes"----
par(mfrow = c(1, 2))
chordDiagram(mat, grid.col = grid.col)
for(si in get.all.sector.index()) {
    # here the index for the grid track is 2
    circos.axis(h = "top", labels.cex = 0.3, major.tick.percentage = 0.2,
    sector.index = si, track.index = 2)
}
text(-0.9, 0.9, "A", cex = 1.5)
# similar as the previous example, but we only plot the grid track
chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
for(si in get.all.sector.index()) {
    circos.axis(h = "top", labels.cex = 0.3, major.tick.percentage = 0.2,
        sector.index = si, track.index = 2)
}

# the second axis as well as the sector labels are added in this track
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    
    if(abs(xplot[2] - xplot[1]) > 20) {
        circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3) # dotted line
        for(p in seq(0.2, 1, by = 0.2)) {
            circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim) + 0.1, 
                p, cex = 0.3, adj = c(0.5, 0), niceFacing = TRUE)
        }
    }
    circos.text(mean(xlim), 1, sector.name, niceFacing = TRUE, adj = c(0.5, 0))
}, bg.border = NA)
circos.clear()
text(-0.9, 0.9, "B", cex = 1.5)
par(mfrow = c(1, 1))

## ----chord_diagram_compare_1, eval = FALSE-------------------------------
#  mat1 = matrix(sample(20, 25, replace = TRUE), 5)
#  
#  gap.degree = c(rep(2, 4), 10, rep(2, 4), 10)
#  circos.clear()
#  circos.par(gap.degree = gap.degree, start.degree = -10/2)
#  chordDiagram(mat1, directional = TRUE, grid.col = rep(1:5, 2))
#  for(si in get.all.sector.index()) {
#      circos.axis(labels.cex = 0.3, major.tick.percentage = 0.2,
#          sector.index = si, track.index = 2)
#  }
#  circos.clear()

## ----chord_diagram_compare_2, eval = FALSE-------------------------------
#  mat2 = mat1 / 2

## ----chord_diagram_compare_3, eval = FALSE-------------------------------
#  percent = sum(abs(mat2)) / sum(abs(mat1))
#  blank.degree = (360 - sum(gap.degree)) * (1 - percent)

## ----chord_diagram_compare_4, eval = FALSE-------------------------------
#  big.gap = (blank.degree - sum(rep(2, 8)))/2
#  gap.degree = c(rep(2, 4), big.gap, rep(2, 4), big.gap)
#  circos.par(gap.degree = gap.degree, start.degree = -big.gap/2)
#  chordDiagram(mat2, directional = TRUE, grid.col = rep(1:5, 2), transparency = 0.5)
#  for(si in get.all.sector.index()) {
#      circos.axis(labels.cex = 0.3, major.tick.percentage = 0.2,
#          sector.index = si, track.index = 2)
#  }
#  circos.clear()

## ----chord_diagram_compare, echo = FALSE, fig.align = "center", out.width = "\\textwidth", out.height = "0.5\\textwidth", fig.width = 7, fig.height = 3.5, fig.cap = "Compare two Chord Diagrams and make them in same scale. bottom matrix has half the values as in the upper matrix."----
par(mfrow = c(1, 2))
mat1 = matrix(sample(20, 25, replace = TRUE), 5)

gap.degree = c(rep(2, 4), 10, rep(2, 4), 10)
circos.clear()
circos.par(gap.degree = gap.degree, start.degree = -10/2)
chordDiagram(mat1, directional = TRUE, grid.col = rep(1:5, 2))
for(si in get.all.sector.index()) {
    circos.axis(labels.cex = 0.3, major.tick.percentage = 0.2,
        sector.index = si, track.index = 2)
}
circos.clear()
text(-0.9, 0.9, "A", cex = 1.5)
mat2 = mat1 / 2
percent = sum(abs(mat2)) / sum(abs(mat1))
blank.degree = (360 - sum(gap.degree)) * (1 - percent)
big.gap = (blank.degree - sum(rep(2, 8)))/2
gap.degree = c(rep(2, 4), big.gap, rep(2, 4), big.gap)
circos.par(gap.degree = gap.degree, start.degree = -big.gap/2)
chordDiagram(mat2, directional = TRUE, grid.col = rep(1:5, 2), transparency = 0.5)
for(si in get.all.sector.index()) {
    circos.axis(labels.cex = 0.3, major.tick.percentage = 0.2,
        sector.index = si, track.index = 2)
}
circos.clear()
text(-0.9, 0.9, "B", cex = 1.5)
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
mat = matrix(rnorm(36), 6, 6)
rownames(mat) = paste0("R", 1:6)
colnames(mat) = paste0("C", 1:6)
mat[2, ] = 1e-10
mat[, 3] = 1e-10

## ----chord_diagram_reduce_1, eval=FALSE----------------------------------
#  chordDiagram(mat)

## ----chord_diagram_reduce_2, eval=FALSE----------------------------------
#  chordDiagram(mat, row.col = rep(c("red", "blue"), 3))

## ----chord_diagram_reduce_3, eval=FALSE----------------------------------
#  chordDiagram(mat, grid.col = rep(c("red", "blue"), 6))
#  circos.clear()

## ----chord_diagram_reduce_4, eval=FALSE----------------------------------
#  circos.par("gap.degree" = rep(c(2, 10), 6))
#  chordDiagram(mat)
#  circos.clear()

## ----chord_diagram_reduce, echo = FALSE, fig.align = "center", out.width = "\\textwidth", fig.cap = "Reduced Chord Diagram with removing tiny sectors. A) notice how sector labels are reduced; B) notice how link colors are reduced; C) notice how grid colors are reduced; D) notice how gap degrees are reduced."----
par(mfrow = c(2, 2))
chordDiagram(mat)
text(-0.9, 0.9, "A", cex = 1.5)
chordDiagram(mat, row.col = rep(c("red", "blue"), 3))
text(-0.9, 0.9, "B", cex = 1.5)
chordDiagram(mat, grid.col = rep(c("red", "blue"), 6))
circos.clear()
text(-0.9, 0.9, "C", cex = 1.5)
circos.par("gap.degree" = rep(c(2, 10), 6))
chordDiagram(mat)
circos.clear()
text(-0.9, 0.9, "D", cex = 1.5)

