\name{dataframe2ts}
\alias{dataframe2ts}
\title{Generate a zoo Object from a suitable data.frame}
\usage{
  dataframe2ts(dataFrame, frequency = "Y", period = "",
    year = "year", var = "share", env = .GlobalEnv,
    addKey = "fameKey")
}
\arguments{
  \item{dataFrame}{input data.frame}

  \item{frequency}{character indicating frequency. Unsharp
  matching, allowed values: "yearly",
  "quarterly","monthly".}

  \item{data.frame}{name of the column that contains the
  period}

  \item{year}{name of the column that contains year}

  \item{var}{name of the column that contains the variable
  of interest}

  \item{env}{environment to assign time series to, default
  '.GlobalEnv'}

  \item{addKey}{additional time series key for second
  referencing system, 'fameKey' is the historic default
  referring to SUNGUARD's time series system FAME.}
}
\description{
  Dont forget that
}
\examples{

}
\author{
  Matthias Bannert
}

