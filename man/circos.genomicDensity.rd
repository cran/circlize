\name{circos.genomicDensity}
\alias{circos.genomicDensity}
\title{
  Calculate and add genomic density track  


}
\description{
  Calculate and add genomic density track  


}
\usage{
circos.genomicDensity(data, ylim.force = FALSE, window.size = NULL, overlap = TRUE,
    col = ifelse(area, "grey", "black"), lwd = par("lwd"), lty = par("lty"), type = "l",
    area = TRUE, area.baseline = NULL, baseline = 0, border = NA, ...)
}
\arguments{
  \item{data}{A bed-file-like data frame or a list of data frames}
  \item{ylim.force}{Whether to force upper bound of \code{ylim} to be 1.}
  \item{window.size}{Pass to \code{\link{genomicDensity}}}
  \item{overlap}{Pass to \code{\link{genomicDensity}}}
  \item{col}{Colors. It should be length of one. If \code{data} is a list of data frames, the length of \code{col} can also be the length of the list.}
  \item{lwd}{Width of lines}
  \item{lty}{Style of lines}
  \item{type}{Type of lines, see \code{\link{circos.lines}}}
  \item{area}{See \code{\link{circos.lines}}}
  \item{area.baseline}{Deprecated, use \code{baseline} instead.}
  \item{baseline}{See \code{\link{circos.lines}}}
  \item{border}{See \code{\link{circos.lines}}}
  \item{...}{Pass to \code{\link{circos.trackPlotRegion}}}

}
\details{
  This function is a high-level graphical function, and it will create a new track. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
