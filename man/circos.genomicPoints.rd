\name{circos.genomicPoints}
\alias{circos.genomicPoints}
\title{
  Add points to a plotting region, specifically for genomic graphics  


}
\description{
  Add points to a plotting region, specifically for genomic graphics  


}
\usage{
circos.genomicPoints(region, value, numeric.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    pch = par("pch"), col = par("col"), cex = par("cex"), ...)
}
\arguments{
  \item{region}{a data frame contains 2 column which is start position and end position}
  \item{value}{a data frame contains values and other information}
  \item{numeric.column}{which column in \code{value} data frame should be taken as y-value. If it is not defined, the whole numeric columns in \code{value} will be taken.}
  \item{sector.index}{pass to \code{\link{circos.points}}}
  \item{track.index}{pass to \code{\link{circos.points}}}
  \item{posTransform}{self-defined functions to transform genomic positions, see \code{\link{posTransform.default}} for explaination}
  \item{col}{color of points. If there is only one numeric column, the length of \code{col} can be either one or number of rows of \code{region}. If there are more than one numeric column, the length of \code{col} can be either one or number of numeric columns. Pass to \code{\link{circos.points}}}
  \item{pch}{type of points. Settings are similar as \code{col}. Pass to \code{\link{circos.points}}}
  \item{cex}{size of points. Settings are similar as \code{col}. Pass to \code{\link{circos.points}}}
  \item{...}{mysterious parameters}

}
\details{
  The function is usually put in \code{panel.fun} when using \code{\link{circos.genomicTrackPlotRegion}}. 


}
