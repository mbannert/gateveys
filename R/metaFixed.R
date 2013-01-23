#' A Class for non-Localized Meta Data
#' 
#' \section{Slots}{
#'  \describe{
#'    \item{\code{tskey}:}{Object of class \code{"character"}, character or logical
#'     time series identifier }
#'    \item{\code{frequency}:}{Object of class \code{"character"}, containing
#'    character or logical allows for unsharp matching, values yearly, quarterly, monthly.}
#'    \item{\code{unit}:}{Object of class \code{"character"}, containing
#'    character or logical unit of measurement}
#'    \item{\code{unitMultiplier}:}{Object of class \code{"character"}, containing
#'    character or logical group e.g. 1000 CHF.}
#'    \item{\code{generatedOn}:}{Object of class \code{"Date"}, containing 
#'    Date or logical Date when the series was created.}
#'    \item{\code{generatedBy}:}{Object of class \code{"character"}, containing
#'    name of the system user that created the series}
#'    \item{\code{questionType}:}{Object of class \code{"character"}, containing
#'    character or logical string, allowed values are mutually exclusive substrings of:
#'    yearly, quarterly, monthly.}
#'    \item{\code{relatedSeries}:}{Object of class \code{"character"}, containing
#'    character or logical root chunk of time series key to
#'    identify related series. Could be extended to a vector of actual series keys
#'    in futute releases.}   
#'  }
#'}
#'
#' @note This is an experimental class to handle localized metadata. 
#' @name metaFixed 
#' @rdname metaFixed
#' @aliases metaFixed-class
#' @exportClass metaFixed
#' @author Matthias Bannert
#' @seealso \code{\link{metaLocalized}}
setClass("metaFixed",
         representation(tskey = "optionalChar",
                        frequency = "optionalChar",
                        status = "optionalChar",
                        unit = "optionalChar",
                        unitMultiplier = "optionalNum",                        
                        generatedOn = "optionalDate",
                        generatedBy = "optionalChar",
                        questionType = "optionalChar",
                        relatedSeries = "vector"
         )
)
