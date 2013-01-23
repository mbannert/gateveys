\name{addFixedMetaData}
\alias{addFixedMetaData}
\title{add non-Localized Meta Information to an existing zoo object}
\usage{
  addFixedMetaData(x, stat = "normal", unit = "percent",
    unitMulti = 1, qType = "qualitative", env = .GlobalEnv)
}
\arguments{
  \item{x}{an object of class zoo}

  \item{li}{list that contains data.frames with tskey
  attribute to reference to}

  \item{stat}{character to display / set status}

  \item{unit}{character that defines unit of measurement}

  \item{unitMulti}{Multiplier of the unit, e.g. 1000 CHF}

  \item{lang}{iso locale that indicates the default
  language for meta information}

  \item{env}{environment to which the resulting time series
  will be appended, default .GlobalEn.}
}
\description{
  add non-Localized Meta Information to an existing zoo
  object
}
\examples{

}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{addLocalizedMetaData}}
}

