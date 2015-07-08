\name{chordDiagram}
\alias{chordDiagram}
\title{
Plot Chord Diagram

}
\description{
Plot Chord Diagram

}
\usage{
chordDiagram(mat, grid.col = NULL, transparency = 0.5,
    col = NULL, row.col = NULL, column.col = NULL, directional = FALSE,
    direction.type = "diffHeight", fromRows = TRUE,
    symmetric = FALSE, keep.diagonal = FALSE, order = NULL, preAllocateTracks = NULL,
    annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
    link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), grid.border = NA,
    diffHeight = 0.04, reduce = 1e-5, link.order = -1,
    link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4),
    link.arr.width = link.arr.length/2,
    link.arr.type = "triangle", link.arr.lty = par("lty"),
    link.arr.lwd = par("lwd"), link.arr.col = par("col"), ...)}
\arguments{

  \item{mat}{A table which represents as a numeric matrix.
  \item{grid.col}{Grid colors which correspond to matrix rows/columns (or sectors). The length of the vector should be either 1 or \code{length(union(rownames(mat), colnames(mat)))}.
  \item{transparency}{Transparency of link colors, 0 means no transparency and 1 means full transparency.
  \item{col}{Colors for links. It can be a matrix which corresponds to \code{mat}, or a function which generate colors 
  \item{row.col}{Colors for links. Links from the same row in \code{mat} will have the same color.
  \item{column.col}{Colors for links. Links from the same column in \code{mat} will have the same color.
  \item{fromRows}{Unequal height of link root is used to represent the link direction.
  \item{directional}{Whether links have directions. The directions are always from rows to columns. If you
  \item{direction.type}{type for representing directions. Can be one or two values in "diffHeight" and "arrows".
  \item{symmetric}{Whether the matrix is symmetric. If the value is set to \code{TRUE}, only
  \item{keep.diagonal}{If the matrix is specified as symmetric, whether keep diagonal for visualization.
  \item{order}{Order of sectors. Default order is \code{union(rownames(mat), colnames(mat))}.
  \item{preAllocateTracks}{Pre-allocate empty tracks before drawing chord diagram. It can be a single number indicating
  \item{annotationTrack}{Which annotation track should be plotted? By default, a track containing sector names and a track
  \item{annotationTrackHeight}{Track height corresponding to values in \code{annotationTrack}.
  \item{link.border}{border for links, single scalar or a matrix with names or a data frame with three columns
  \item{link.lwd}{width for link borders, single scalar or a matrix with names or a data frame with three columns
  \item{link.lty}{style for link borders, single scalar or a matrix with names or a data frame with three columns
  \item{grid.border}{border for grids. If it is \code{NULL}, the border color is same as grid color
  \item{diffHeight}{The difference of height between two 'roots' if \code{directional} is set to \code{TRUE}. If the value is set to
  \item{reduce}{if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.
  \item{link.order}{order of links in single sector. The value is a length-two vector which 
  \item{link.arr.length}{pass to \code{\link{circos.link}}, same settings as \code{link.lwd}.
  \item{link.arr.width}{pass to \code{\link[shape]{Arrowhead}}, same settings as \code{link.lwd}.
  \item{link.arr.type}{pass to \code{\link{circos.link}}, same settings as \code{link.lwd}. Default value is \code{triangle}.
  \item{link.arr.col}{color or the single line link which is put in the center of the belt, same settings as \code{link.lwd}.
  \item{link.arr.lwd}{line width ofthe single line link which is put in the center of the belt, same settings as \code{link.lwd}.
  \item{link.arr.lty}{line type of the single line link which is put in the center of the belt, same settings as \code{link.lwd}.
  \item{...}{pass to \code{\link{circos.link}}
}
\details{
Chord diagram is a way to visualize numeric tables ( \url{http://circos.ca/intro/tabular_visualization/} ), especially useful
when the table represents information of directional relations. This function
visualize tables in a circular way.

Sectors of the circos plot is \code{union(rownames(mat), colnames(mat))}. If there is no rowname or colname, the function will
assign names for it ("R1", "R2", ... for row names, "C1", "C2", ... for column names).

This function is flexible and contains some settings that may be a little difficult to understand. 
Please refer to vignette for better explanation.

}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.}
\examples{
\dontrun{

############### example 1 ######################################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]

### basic settings
par(mfrow = c(3, 2))
par(mar = c(1, 1, 1, 1))

chordDiagram(mat)
circos.clear()

circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat)
circos.clear()

circos.par(start.degree = 90)
chordDiagram(mat)
circos.clear()

chordDiagram(mat, order = c("A", "B", "a", "C", "D", "b", "E", "F", "c"))

chordDiagram(mat, directional = TRUE)
chordDiagram(mat, directional = TRUE, diffHeight = 0.06)

circos.clear()

################ example 2 ###############################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]


### colors settings
rand_color = function(n, alpha = 1) {
    return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
}

par(mfrow = c(3, 3))
par(mar = c(1, 1, 1, 1))
grid.col = NULL
grid.col[letters[1:3]] = c("red", "green", "blue")
grid.col[LETTERS[1:6]] = "grey"
chordDiagram(mat, grid.col = grid.col)
chordDiagram(mat, grid.col = grid.col, transparency = 0.5)
col_mat = rand_color(length(mat), alpha = 0.5)
dim(col_mat) = dim(mat)
chordDiagram(mat, grid.col = grid.col, col = col_mat)
chordDiagram(mat, grid.col = grid.col,
    col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)),
                     rev(heat.colors(11))), transparency = 0.5)

chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
circos.clear()

}}