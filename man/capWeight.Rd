\name{capWeight}
\alias{capWeight}
\title{Apply an exponent function if value beyond max threshold}
\usage{
  capWeight(weight, threshold = 500, exponent = 0.7)
}
\arguments{
  \item{weight}{input numeric vector}

  \item{threshold}{input numeric giving the threshold
  beyond which exp function should be applied}

  \item{exponent}{input numeric giving the exponent,
  typically below one as influence of big weights should be
  declined}
}
\value{
  numeric vector of capped weights
}
\description{
  This function adjusts the size of values in a numeric
  vector once value go beyond a certain threshold. The
  adjustment is done by an exponential function with the
  default exponent 0.7. In the context of business tendency
  surveys this function is often used to lower the
  influence of very large companies when employment based
  weighting is used.
}
\examples{
# vector of weights
weights <- c(1001,2,3,400,5,700,9,1,800,79223)
# weights below the thresholds are not affected
# by capWeight
capWeight(weights)
#
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{setSizeClass}}
}

