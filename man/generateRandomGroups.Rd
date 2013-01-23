\name{generateRandomGroups}
\alias{generateRandomGroups}
\title{generateRandomGroups}
\usage{
  generateRandomGroups(n, uid, variables = LETTERS)
}
\arguments{
  \item{n}{number of groups, ranges from 1-26 as groups are
  denoted by letters}

  \item{uid}{vector of uid to assign random groups to}
}
\value{
  data.frame that can be merged with a dataset
}
\description{
  generateRandomGroups
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
\seealso{
  \code{\link{generateSamplePanel}}
}

