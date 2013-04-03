\name{addLocalizedMetaData}
\alias{addLocalizedMetaData}
\title{add localized meta data to an exisiting zoo object}
\usage{
  addLocalizedMetaData(x, attrName = "de",
    qDict = questionDescription.de, aDict = DLU_ANTW,
    wInfo = "Gewichtung mit Beschäftigten (capped)",
    desc = "Dienstleistungsumfrage basierend auf NOGA08",
    env = .GlobalEnv)
}
\arguments{
  \item{x}{any R object to assign meta data to, typically a
  zoo object}

  \item{attrName}{iso name string that indicated the
  localization}

  \item{wInfo}{character string that contains some
  weighting information, typically 'weighted', 'unweighted'
  or ''.}

  \item{qDict}{a data.frame that contains some meta
  descriptions for questions. Might be read from an
  external source.}

  \item{env}{environment to assign time series too,
  .GlobalEnv is default.}

  \item{desc}{language specific description}
}
\description{
  add language specific meta information to a time series
  object. Typically data is added to zoo objects. data
}
\examples{

}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{addFixedMetaData}}
}

