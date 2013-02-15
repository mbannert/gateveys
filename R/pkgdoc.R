#' An R Toolbox for Business Tendency Survey Researchers
#' 
#' This package offers functions to facilitate the aggregation 
#' of qualitative survey data. Is it build to handle panel data, 
#' i.e. cross-sectional and longitudinal at the same time. 
#' Originally it was developed to aggregate Business Tendency
#' Surveys (BTS), but it should work for the aggregation of any other
#' qualitative survey, too. In other words: this package
#' helps to create a reproducible production process
#' from micro data (i.e. single participant answers) to 
#' macro time series (i.e. weighted grouped shares observed over time)
#' 
#' @note
#' Special thanks go out to Markus Graf who helped with a non-public
#' predecessor of this package. Thanks for bringing your ideas and   
#' R skills to the table. Further thanks to the people 
#' @@http://stackoverflow.com/questions/tagged/r. 
#' You guys know who you are!
#' 
#'  @import data.table
#'  @import methods
#'  @import ggplot2
#'  @import stringr
#'  @import zoo
#'  @exportPattern "^[^\\.]"
#'  @author Matthias Bannert <bannert@@kof.ethz>
#'  @docType package
#'  @name gateveys
NULL