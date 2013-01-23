\name{drawFromMixed}
\alias{drawFromMixed}
\title{draw random variables from a mixed distribution}
\usage{
  drawFromMixed(n, li, p)
}
\arguments{
  \item{n}{number of total draws from all distributions}

  \item{li}{named list of random distribution functions
  that contains vectors of with corresponding function
  parameters}

  \item{probs}{vector of drawing probabilities from
  different functions which has to be the same length as
  list}
}
\value{
  vec vector of random variables
}
\description{
  draw random variables from a mixed distribution
}
\examples{
# n is the number of draws from ALL distributions
w <- drawFromMixed(100, list(rnorm = list(mean = 1000, sd = 250),
                             rexp = list(),
                             rchisq = list(df = 10)),
                             c(0.1, 0.5, 0.4))

w1 <- unlist(w) 
names(w1) <- NULL
w1
}
\author{
  Matthias Bannert
}

