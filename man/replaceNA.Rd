\name{replaceNA}
\alias{replaceNA}
\title{Replace NA in categorical data}
\usage{
  replaceNA(x, subst)
}
\arguments{
  \item{x}{vector that possibly contains NAs}

  \item{subst}{character string that contains substitution
  of NA.}
}
\value{
  factor that contains a level denoted by 'subst' instead
  of NAs.
}
\description{
  This functions replaces NA by the string subst. The
  function can easily be used within lapply strutctures.
}
\examples{
# some vector that contains NAs
someVector <- c(1,2,NA,NA,3,1,2,2,0,9,4,NA,9,2)

# replace all NAs by string and turn 
# variable into factor
replaceNA(someVector,"empty")

# turning NA into a category is common use case
table(someVector)
table(someVector,useNA="alw")
table(replaceNA(someVector,"empty"))
}
\author{
  Matthias Bannert
}

