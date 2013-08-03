\name{read.cytoband}
\alias{read.cytoband}
\title{
 Read cytoband data


}
\description{
  Read cytoband data


}
\usage{
read.cytoband(file = paste(system.file(package = "circlize"), 
    "/extdata/cytoBand.txt", sep=""))
}
\arguments{
  \item{file}{path of the uncompressed cytoband file
}
}
\details{
  The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome.


}
\value{
  \describe{
    \item{df}{Original data frame for cytoband data}
    \item{chromosome}{sorted chromosome names}
    \item{chr.len}{length of chromosomes. Order are same as \code{chromosome}}
  }
}