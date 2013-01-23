\name{plotTs}
\alias{plotTs}
\title{plot (multiple) time series conveniently}
\usage{
  plotTs(..., pal = eth.colors(), lg = T,
    llabel = "Legende", lang = "de_CH")
}
\arguments{
  \item{...}{vector of time series objects}

  \item{pal}{character string containing color palette}

  \item{lg}{logical paramater that indicates whether to use
  a language specfic legend}

  \item{llabel}{character string to influence title of the
  legend}

  \item{lang}{character string that indicates which
  language should be use in the meta description.}
}
\description{
  This function plots one or more time series into the same
  plot. It uses createGeomLine to create multiple layers of
  line geoms. Just input several time series objects
  separated by commas to this function to obtan all series
  in one plot.
}
\author{
  Matthias Banert
}
\seealso{
  \code{\link{createGeomLine}}
}

