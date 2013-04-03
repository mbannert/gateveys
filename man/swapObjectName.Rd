\name{swapObjectName}
\alias{swapObjectName}
\title{Swap object Name based on two attribute names}
\usage{
  swapObjectName(x, attr1 = "tskey", attr2 = "fameKey")
}
\arguments{
  \item{x}{R object or character name of an object}

  \item{attr1}{name of the first attribute. Default is
  'tskey'. Typically the name of this attribute is equal to
  the initial object name}

  \item{attr2}{name of the second attribute. Object will
  change its name to the value of this second attribute.
  Default value is 'fameKey'}
}
\description{
  This function swaps the name of an R object between two
  of its attributes. Objects need to have at least to
  character objects to use this function. Typically one
  character object should be equal to the objects original
  name in order to be able to swap back to the old name.
}
\author{
  Matthias Bannert
}

