\name{for.setattr}
\alias{for.setattr}
\title{Add tskey attribute to every data.frame in a list of data.frames}
\usage{
  for.setattr(li, key = "tskey")
}
\arguments{
  \item{li}{named list of data.frames}
}
\description{
  This function appends an attribute named tskey to every
  data.frame in a given named list of data.frames. List
  names are used as tskey values. This small function could
  also be integrated to the big linearize function
}
\examples{
# first create a nested list
nestedList <- list(A=list(a=c(1,2,3),b=c(3,2,1)),
                   B=list(a=c(4,5,6),
                   b=c(7,8,9),
                   d=list(a1="test",
                          b2="anther test")
                   ))

# call the linearize function for lists
linearList <- linearizeNestedList(nestedList)

# set the attribute "tskey" to to every list element based on its name
for.setattr(linearList)
linearList
}
\author{
  Matthias Bannert
}

