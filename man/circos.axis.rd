\name{circos.axis}
\alias{circos.axis}
\title{
  Draw x-axis  


}
\description{
  Draw x-axis  


}
\usage{
circos.axis(h = "top", major.at = NULL, labels = TRUE, major.tick = TRUE,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    labels.font = par("font"), labels.cex = par("cex"), labels.direction = "default",
    direction = c("outside", "inside"), minor.ticks = 4,
    major.tick.percentage = 0.1, labels.away.percentage = 0.05, lwd = par("lwd"))
}
\arguments{
  \item{h}{position of the x-axis, can be "top", "bottom" or a numeric value}
  \item{major.at}{If it is numeric vector, it identifies the poisitions of the major ticks. It can exceed \code{xlim} value and the exceeding part would be trimmed automatically. If it is \code{NULL}, it would be calculated by \code{\link[base]{pretty}} (about every 10 degrees there is a major tick).}
  \item{labels}{labels of the major ticks. Also, the exceeding part would be trimmed automatically.}
  \item{major.tick}{Whether to draw major tick. If it is set to \code{FALSE}, there would be no minor ticks either. }
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{labels.font}{font style for the axis labels}
  \item{labels.cex}{font size for the axis labels}
  \item{labels.direction}{font direction for the axis labels, shoud be in (\code{default}, \code{default2}, \code{vertical_left}, \code{vertical_right}, \code{horizontal}, \code{arc})}
  \item{direction}{whether the axis ticks point to the outside or inside of the circle.}
  \item{minor.ticks}{Number of minor ticks between two close major ticks.}
  \item{major.tick.percentage}{Length of the major ticks. It is the percentage to the height of the cell.}
  \item{labels.away.percentage}{The distance for the axis labels to the major ticks. It is the percentage to the height of the cell.}
  \item{lwd}{line width for ticks}

}
\details{
  It can only draw axis on x-direction. 


}
\examples{

library(circlize)

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.1,
    bg.border = NA, panel.fun = function(x, y) {
        circos.text(5, 10, get.cell.meta.data("sector.index"))
})

circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.axis(sector.index = "a")
circos.axis(sector.index = "b", direction = "inside",
    labels.direction = "default2")
circos.axis(sector.index = "c", h = "bottom")
circos.axis(sector.index = "d", h = "bottom", direction = "inside")
circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9),
    labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9),
    labels = c("a", "c", "e", "g", "f"), major.tick = FALSE)
circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9), 
    labels = c("a", "c", "e", "g", "f"), major.tick.percentage = 0.3,
    labels.away.percentage = 0.2, minor.ticks = 2,
    labels.direction = "vertical_right")
circos.clear()

\dontrun{
library(circlize)
factors = letters[1]
par(mar = c(1, 1, 1, 1))

while(1) {
    circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0),
        "start.degree" = 90)
    circos.initialize(factors = factors, xlim = c(0, 12))
    circos.trackPlotRegion(factors = factors, ylim = c(0, 1),
        bg.border = NA)
    circos.axis(sector.index = "a", major.at = 0:12, labels = "",
        direction = "inside", labels.cex = 1.5, major.tick.percentage = 0.3)
    circos.text(1:12, rep(0.5, 12), 1:12, direction = "horizontal")

    current.time = as.POSIXlt(Sys.time())
    sec = ceiling(current.time$sec)
    min = current.time$min
    hour = current.time$hour

    sec.degree = 90 - sec/60 * 360
    arrows(0, 0, cos(sec.degree/180*pi)*0.8, sin(sec.degree/180*pi)*0.8)

    min.degree = 90 - min/60 * 360
    arrows(0, 0, cos(min.degree/180*pi)*0.7, sin(min.degree/180*pi)*0.7,
        lwd = 2)   

    hour.degree = 90 - hour/12 * 360 - min/60 * 360/12
    arrows(0, 0, cos(hour.degree/180*pi)*0.4, sin(hour.degree/180*pi)*0.4, 
        lwd = 2)

    circos.clear()
    Sys.sleep(1)
}
}

}
