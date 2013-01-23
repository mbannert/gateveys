\name{generateRandomNAs}
\alias{generateRandomNAs}
\title{add random NAs to a data.frame}
\usage{
  generateRandomNAs(df, protected, numberOfNAs, seed)
}
\arguments{
  \item{df}{input}

  \item{protected}{are cols of the data.frames which need
  to be proteced from random NAs}

  \item{number}{of NAs that should be introduced.}

  \item{seed}{for reproducible random number generation}
}
\value{
  data.frame that contains several random NAs within its
  questions
}
\description{
  add random NAs to a data.frame
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

