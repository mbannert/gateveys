\name{metaLocalized}
\alias{metaLocalized}
\alias{metaLocalized-class}
\title{Class for Localized Meta Information}
\description{
  This is basically a template for a class, that organizes
  meta attributes that are language variant. You could add
  multiple attributes of this class to a time series to
  have meta information in multiple languages.
}
\details{
  \section{Slots}{ \describe{ \item{\code{title}:}{Object
  of class \code{"character"}, character or logical
  descriptive title } \item{\code{selectedItem}:}{Object of
  class \code{"character"}, character or logical
  categorical item of interest.}
  \item{\code{description}:}{Object of class
  \code{"character"}, containing character or logical
  elaborate description} \item{\code{aLevel}:}{Object of
  class \code{"character"}, containing character or logical
  aggregation Level} \item{\code{selectedGroup}:}{Object of
  class \code{"character"}, containing character or logical
  group that was actually selected in this time series on a
  particular aggregation Level.}
  \item{\code{survey}:}{Object of class \code{"character"},
  containing character or logical constant indicating the
  survey the questions stem from.}
  \item{\code{questionWording}:}{Object of class
  \code{"character"}, containing character or logical
  wording of the question. Can be taken from a dictionary
  assignments file if available.}
  \item{\code{itemLevels}:}{Object of class
  \code{"character"}, containing character or logical all
  possible levels of answer.}
  \item{\code{weightingInformation}:}{Object of class
  \code{"character"}, containing character or logical
  information on the weighting that was used in the
  aggregation process.} } }
}
\note{
  This is an experimental class to handle localized
  metadata.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{metaFixed}}
}

