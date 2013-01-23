\name{countNAs}
\alias{countNAs}
\title{Count NAs}
\usage{
  countNAs(x)
}
\arguments{
  \item{x}{vector that potentially contains NAs}
}
\value{
  y line named row vector
}
\description{
  This function counts NAs. It is designed to find NAs per
  column in survey based data. Best used when used with
  ldply.
}
\examples{
dframe <- data.frame(test_vector_1 = c(1,2,324,5,1,2),
              test_vector_2 = c(NA,324,NA,1,NA,NA),
                     test_vector_3=c(NA,NA,NA,NA,NA,NA))

# single calls
countNAs(dframe$test_vector_1)  
countNAs(dframe$test_vector_2)  
countNAs(dframe$test_vector_3)  

# if plyr is loaded, try
# require(plyr)
# ldply(dframe,countNAs)
# can easily rename that because
# FALSE is always first with countNAs



}
\author{
  Matthias Bannert
}

