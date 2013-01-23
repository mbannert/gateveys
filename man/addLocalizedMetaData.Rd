\name{addLocalizedMetaData}
\alias{addLocalizedMetaData}
\title{add localized meta data to an exisiting zoo object}
\usage{
  addLocalizedMetaData(x, li = listOfSeries,
    attrName = "de_CH", srvy = SURVEY, wInfo = "weighted",
    qDict = questionDescription.de, env = .GlobalEnv,
    langChunk1 = "of participants who chose",
    langChunk2 = "when answering")
}
\arguments{
  \item{x}{any R object to assign meta data to, typically a
  zoo object}

  \item{attrName}{iso name string that indicated the
  localization}

  \item{srvy}{character constant that represents the
  analyzed survey}

  \item{wInfo}{character string that contains some
  weighting information, typically 'weighted', 'unweighted'
  or ''.}

  \item{qDict}{a data.frame that contains some meta
  descriptions for questions. Might be read from an
  external source.}

  \item{env}{environment to assign time series too,
  .GlobalEnv is default.}

  \item{langChunk1}{language specific chunk 1 for
  description}

  \item{langChunk2}{language specific chunk 2 for
  description}
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

