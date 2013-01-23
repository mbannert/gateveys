\name{checkKeys}
\alias{checkKeys}
\title{Turn Names Generated from the Aggregation Process into valid tskeys}
\usage{
  checkKeys(nms, country, provider, survey, li)
}
\arguments{
  \item{nms}{character vector that typically contains the
  names of a list of data.frames}

  \item{country}{character country code}

  \item{provider}{character string to identify data
  provider in a country}

  \item{survey}{character string to identify the data}
}
\value{
  character string containing a valid gateveys tskey
}
\description{
  This function is yet quite specific and works only with a
  list of timeseries storen in multiple separate
  date.frames. Currently the function has no example yet.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{for.setattr},\link{linearizeNestedList}}
}

