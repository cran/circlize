## ----echo = FALSE--------------------------------------------------------
library(knitr)
opts_chunk$set(fig.pos = "")

library(circlize)
circos.initialize = function(...) {
    circos.par(unit.circle.segments = 400)
    circlize::circos.initialize(...)
}

## ----eval = FALSE--------------------------------------------------------
#  library(ape)
#  data(bird.orders)
#  hc = as.hclust(bird.orders)

## ----echo = FALSE--------------------------------------------------------
load(paste0(system.file(package = "circlize"), "/extdata/bird.orders.RData"))

## ------------------------------------------------------------------------
labels = hc$labels  # name of birds
ct = cutree(hc, 6)  # cut tree into 6 pieces
n = length(labels)  # number of bird species
dgm = as.dendrogram(hc)

## ------------------------------------------------------------------------
dgm
attributes(dgm)
length(dgm)
dgm[[1]]
dgm[[2]]

## ----phylogenetic_tree_part1, eval = FALSE-------------------------------
#  library(circlize)
#  par(mar = c(1, 1, 1, 1))
#  circos.par(cell.padding = c(0, 0, 0, 0))
#  circos.initialize(factors = "a", xlim = c(0, n)) # only one sector
#  maxy = attr(dgm, "height")  # maximum height of the trees
#  circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.3,
#      panel.fun = function(x, y) {
#          for(i in seq_len(n)) {
#              circos.text(i-0.5, 0, labels[i], adj = c(0, 0.5),
#                  facing = "clockwise", niceFacing = TRUE,
#                  col = ct[labels[i]], cex = 0.7)
#          }
#  })

## ------------------------------------------------------------------------
# == param
# -dend a `dendogram` object
# -maxy the maximum height of the tree is a global attribute,
#       so here it is set as an argument
circos.dendrogram = function(dend, maxy = attr(dend, "height")) {
    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5 # leaves are places at x = 0.5, 1.5, ..., n - 0.5
    names(x) = labels
    
    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }
    
    draw.d = function(dend, maxy) {
        leaf = attr(dend, "leaf")
        d1 = dend[[1]]  # child tree 1
        d2 = dend[[2]]  # child tree 2
        height = attr(dend, "height")
        midpoint = attr(dend, "midpoint")
        
        if(is.leaf(d1)) {
            x1 = x[as.character(attr(d1, "label"))]
        } else {
            x1 = attr(d1, "midpoint") + x[as.character(labels(d1))[1]]
        }
        y1 = attr(d1, "height")
        
        if(is.leaf(d2)) {
            x2 = x[as.character(attr(d2, "label"))]
        } else {
            x2 = attr(d2, "midpoint") + x[as.character(labels(d2))[1]]
        }
        y2 = attr(d2, "height")
        
        # plot the connection line
        circos.lines(c(x1, x1), maxy - c(y1, height), straight = TRUE)
        circos.lines(c(x1, x2), maxy - c(height, height))
        circos.lines(c(x2, x2), maxy - c(y2, height), straight = TRUE)
        
        # do it recursively
        if(!is.leaf(d1)) {
            draw.d(d1, maxy)
        }
        if(!is.leaf(d2)) {
            draw.d(d2, maxy)
        }
    }
    
    draw.d(dend, maxy)
}

## ----phylogenetic_tree_part2, eval = FALSE-------------------------------
#  circos.trackPlotRegion(ylim = c(0, maxy), bg.border = NA,
#      track.height = 0.4, panel.fun = function(x, y) {
#          circos.dendrogram(dgm, maxy)
#  })
#  
#  circos.clear()

## ----phylogenetic_tree, echo = FALSE, fig.align = "center", out.width = "0.6\\textwidth", out.height = "1.2\\textwidth", fig.width = 6, fig.height = 12, fig.cap = "A simple phylogenetic tree. Top: circular style; Bottom: normal style."----
par(mar = c(1, 1, 1, 1), mfrow = c(2, 1))
library(circlize)
par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0))
circos.initialize(factors = "a", xlim = c(0, n)) # only one sector
maxy = attr(dgm, "height")  # maximum height of the trees
circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.3, 
    panel.fun = function(x, y) {
        for(i in seq_len(n)) {
            circos.text(i-0.5, 0, labels[i], adj = c(0, 0.5), 
                facing = "clockwise", niceFacing = TRUE,
                col = ct[labels[i]], cex = 0.7)
        }
})
circos.trackPlotRegion(ylim = c(0, maxy), bg.border = NA, 
    track.height = 0.4, panel.fun = function(x, y) {
        circos.dendrogram(dgm, maxy)
})

circos.clear()
par(mar = c(8, 4, 4, 1))
plot(dgm)

