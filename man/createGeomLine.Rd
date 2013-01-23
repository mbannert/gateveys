\name{createGeomLine}
\alias{createGeomLine}
\title{Create geoms of type line to plot time series with ggplot}
\usage{
  createGeomLine(sts, lang = "de_CH")
}
\arguments{
  \item{sts}{zoo survey time series object with attribute
  metaLocale.}

  \item{lang}{character string indicating which language
  will be used in the labels.}
}
\value{
  geom a ggplot layer containing a line for use in a chart.
}
\description{
  This function creates geoms for ggplot2. Time series with
  metainformation can be used as arguments to this function
  in order to create plots with metainformation. Though it
  can be used separately for more custom plot,
  createGeomLine is typically used with the convenience
  plotting function plotTs.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{plotTs}}
}

