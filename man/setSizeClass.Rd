\name{setSizeClass}
\alias{setSizeClass}
\title{Add Size Class based on weights}
\usage{
  setSizeClass(x, thresholdList, sector = "all",
    sizeColumn = "size", sectorColumn = "sector",
    resultColumn = "sizeClass", minimalClass = "S")
}
\arguments{
  \item{x}{data.frame that contains the dataset}

  \item{thresholdList}{List of group specific threshold
  assignments}

  \item{sector}{}

  \item{sizeColumn}{}

  \item{sectorColumn}{}

  \item{resultColumn}{}

  \item{minimalClass}{}
}
\value{
  factor containing size classes defined in thresholdList
  and minimalClass. e.g.: S,M,L
}
\description{
  Add Size Class based on weights
}
\examples{
# generate survey likerandom sample
srvyData <- generateSamplePanel(500,3,"quarterly",
                    c(1999,1),
                    c(1999,1),
                    weight=round(runif(500,2,80)))

# add a sector, cause function supports different 
# thresholdLists for different sectors
srvyData$sector <- "A"

# create classes based on weight
srvyData <- setSizeClass(srvyData,
                              thresholdList=list(M=20,L=40),
                              sizeColumn="weight", 
                              sectorColumn="sector")
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{setSizeClass}, \link{generateSamplePanel}}
}

