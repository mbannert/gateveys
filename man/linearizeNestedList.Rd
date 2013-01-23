\name{linearizeNestedList}
\alias{linearizeNestedList}
\title{linearize nested lists}
\usage{
  linearizeNestedList(nList, linearizeDataFrames = FALSE,
    nameSep = ".", forceNames = FALSE)
}
\arguments{
  \item{nList}{input nested List}

  \item{linearizeDataFrames}{logical to indicate whether
  data.frames should be linearized too}

  \item{nameSep}{character to seperate different list
  elements}

  \item{forceNames}{logical to indicate whether original
  names should be removed and sequential numbers should be
  used}
}
\value{
  a linear list whose names hold the information containing
  in the nesting of the input list
}
\description{
  This function un-nests a list and stores the result into
  a named list with all elements on the first depth level.
  The function is recursive and its input is only limited
  by the maximum nesting level R allows for.
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
  Akhil Behl <akhilsbehl@gmail.com>, Matthias Bannert
  <bannert@kof.ethz.ch>
}
\references{
  This function was originally created by Akhil Behl and
  was only slightly modified by the author of this package.
  The original code can be found on his website:
  https://sites.google.com/site/akhilsbehl/geekspace/articles/r/linearize_nested_lists_in_r
}

