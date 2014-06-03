\name{circos.info}
\alias{circos.info}
\title{
  Get information of the circos plot  


}
\description{
  Get information of the circos plot  


}
\usage{
circos.info(sector.index = NULL, track.index = NULL, plot = FALSE)
}
\arguments{
  \item{sector.index}{which sectors you want to look at}
  \item{track.index}{which tracks you want to look at}
  \item{plot}{whether add information on the plot}

}
\details{
  It tells you the basic parameters for sectors/tracks/cells. If both \code{sector.index} and \code{track.index} are set to \code{NULL}, the function would print index for  all sectors and all tracks. If \code{sector.index} and/or \code{track.index} are set, the function would print xlim and ylim in the data coordinate for every cell. Also, the function will print index for your current sector and current track.  

  If \code{plot} is set to \code{TRUE}, the function will draw the index of the sector and the track  for each cell on the figure. 


}
