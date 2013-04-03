\name{setSizeClass}
\alias{setSizeClass}
\title{Set Size Class based on a Quantitative Variable}
\usage{
  setSizeClass(df, thresholdList, sectorColumn = "all",
    resultColumn = "sClass", size = "BESCHAEFTIGTE",
    minClassLabel = "S")
}
\arguments{
  \item{df}{data.frame that contains the basic dataset}

  \item{thresholdList}{list of thresholds, list elements
  can be vectors if different groups have different
  thresholds}

  \item{sectorColumn}{character string indicating the
  column that specifies the groups at across which
  thresholds could be different. Default is 'all' meaning
  all groups have the same thresholds.}

  \item{resultColumn}{character string containing the name
  of the resulting column}

  \item{size}{character string containing the name of the
  quantitative variable.}
}
\description{
  This function is used in survey based research to create
  size groups based on a quantitative variable like the
  number of employees. Users can specify multiple
  thresholds that may vary over groups.
}
\author{
  Matthias Bannert
}

