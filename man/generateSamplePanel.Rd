\name{generateSamplePanel}
\alias{generateSamplePanel}
\title{generate sample panel}
\usage{
  generateSamplePanel(n, q, freq, start, end, seed = 1,
    rng = 1:3, groupList = NA, weight = 1)
}
\arguments{
  \item{n}{number of observations}

  \item{q}{number of questions}

  \item{freq}{character string indicating frequency, can be
  "annually", "quarterly", "monthly". Supports fuzzy
  unabmigous strings.}

  \item{start}{vector containing year,period}

  \item{end}{vector containing year,period'}

  \item{seed}{for reproducible random number generation}

  \item{rng}{of the categorical scale default 1-3}

  \item{weight}{vector of weights, if of greater length
  than n, a sample of length n is drawn, default is set to
  1 (equal weights)}
}
\value{
  data.frame that represents BTS typical data
}
\description{
  generate sample panel
}
\examples{
# generate survey likerandom sample
srvyData <- generateSamplePanel(500,3,"quarterly",
                                c(1999,1),
                                c(1999,1),
                                weight=round(runif(500,2,80)))


# generate 
grps <- generateRandomGroups(3,1:500,LETTERS)

# merge group information by uid
srvyWgroups <- merge(grps,srvyData,by="uid")

# add some NAs (to simulate item non-response)
# check which cols should be protected from 
# being hit by random NAs
head(srvyWgroups)
randomPanel <- generateRandomNAs(srvyWgroups,c(1,2,3,4),50,1)
randomPanel
}
\author{
  Matthias Bannert
}

