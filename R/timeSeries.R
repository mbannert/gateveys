#' Generate a zoo Object from a suitable data.frame
#' 
#' Dont forget that 
#' 
#' @author Matthias Bannert
#' @param dataFrame input data.frame
#' @param frequency character indicating frequency. Unsharp matching, allowed
#' values: "yearly", "quarterly","monthly". 
#' @param data.frame name of the column that contains the period
#' @param year name of the column that contains year
#' @param var name of the column that contains the variable of interest
#' @param env environment to assign time series to, default '.GlobalEnv'
#' @example examples/dataframe2tsExample.R
dataframe2ts <- function(dataFrame,frequency="Y",period="",year="year",
                         var="share",env=.GlobalEnv){
  # make sure data.tables are turned to data.frames
  dataFrame <- as.data.frame(dataFrame)
  if(!is.na(pmatch(tolower(frequency),"quarterly"))){
    idx <- as.yearqtr(paste(dataFrame[,year],"Q",dataFrame[,period],sep=""))
    idx <- as.Date(idx)
  }
  if(exists("zoo")){
    res <- zoo(dataFrame[,var],idx)
    attr(res,'frequency') <- frequency
    attr(res,'tskey') <- attr(dataFrame,"tskey")
    assign(attr(dataFrame,"tskey"),res,envir=env)
    out <- paste("assigned time series",attr(dataFrame,"tskey"),"to",
                 "environment '",
                 deparse(substitute(env)),"' .")
    return(out)
  } else {
    res <- "package zoo not available, need to implement this function to work
    with standard ts. Please install zoo til then."
    return(res)
  }
}


#' add non-Localized Meta Information to an existing zoo object
#' 
#' @author Matthias Bannert
#' @param x an object of class zoo
#' @param li list that contains data.frames with tskey attribute to reference to
#' @param stat character to display / set status
#' @param unit character that defines unit of measurement
#' @param unitMulti Multiplier of the unit, e.g. 1000 CHF
#' @param lang iso locale that indicates the default language for
#' meta information
#' @param env environment to which the resulting time series will be 
#' appended, default .GlobalEn.
#' @seealso \code{\link{addLocalizedMetaData}}
#' @example examples/addMetaDataExample.R
addFixedMetaData <- function(x,stat="normal",
                                   unit="percent",
                                   unitMulti=1,qType="qualitative",
                                   env=.GlobalEnv){
  tsname <- ifelse(is.character(x),x,deparse(substitute(x)))
  
  # ifelse would not work cause condition is of length 1 !!
  if (is.character(x)) x <-  get(x)
    else x <- x  
  m <- new("metaFixed",
           tskey = attr(x,"tskey"),
           frequency = attr(x,"frequency"),
           status = stat,
           unit = unit,
           unitMultiplier = unitMulti,
           generatedOn = Sys.Date(),
           generatedBy = Sys.getenv('USER'),
           questionType = qType,
           relatedSeries = strsplit(tsname,"\\.item")[[1]][1])
  metaFixed(x) <- m
  # remove that duplicate information
  attr(x,"tskey") <- NULL
  attr(x,"frequency") <- NULL
  assign(tsname,x,envir=env)
  out <- paste(tsname,"successfully updated with fixed meta information.")
  return(out)
  
}

#' add localized meta data to an exisiting zoo object
#' 
#' add language specific meta information to a time series object.
#' Typically data is added to zoo objects. data 
#' 
#' @author Matthias Bannert
#' @param x any R object to assign meta data to, typically a zoo object
#' @param attrName iso name string that indicated the localization
#' @param srvy character constant that represents the analyzed survey
#' @param wInfo character string that contains some weighting information,
#' typically 'weighted', 'unweighted' or ''.
#' @param qDict a data.frame that contains some meta descriptions for 
#' questions. Might be read from an external source.
#' @param env environment to assign time series too, .GlobalEnv is default.
#' @param langChunk1 language specific chunk 1 for description
#' @param langChunk2 language specific chunk 2 for description
#' @seealso \code{\link{addFixedMetaData}}
#' @example examples/addMetaDataExample.R
addLocalizedMetaData <- function(x,li=listOfSeries,
                                       attrName="de_CH",srvy=SURVEY,
                                       wInfo="weighted",
                                       qDict=questionDescription.de,
                                       env=.GlobalEnv,
                                       langChunk1="of participants who chose",
                                       langChunk2="when answering"){
  # define a couple of variables for use instance constructor below 
  tsname <- ifelse(is.character(x),x,deparse(substitute(x)))
  # ifelse would not work cause condition is of length 1 !!
  if (is.character(x)) x <-  get(x)
  else x <- x   
  chunks <- strsplit(tsname,"\\.")[[1]]
  vars <- names(li[[tsname]])
  lv <- length(vars)
  l <- length(chunks)
  m <- new("metaLocalized",
           # could be generated from question Wording,
           # maybe build a dict for that
           # what to do with empty fields
           title = qDict[qDict$qkey == chunks[5],"questionShortLabel"],
           selectedItem = chunks[l],
           # KOF Survey, employment weighted shares question item
           description = paste(wInfo,vars[lv],langChunk1,
                               chunks[l],langChunk2,chunks[5],",",
                               chunks[2],srvy),
           aLevel = chunks[4], 
           selectedGroup = chunks[6],
           survey = SURVEY,
           questionWording = qDict[qDict$qkey == chunks[5],"questionWording"],
           # need factors here from the very beginning
           itemLevels = levels(li[[tsname]][,get(chunks[5])]),
           # a manual description 
           weightingInformation = wInfo)
  attr(x,paste("metaLocalized",attrName,sep=".")) <- m
  assign(tsname, x, envir = env)
  out <- paste(tsname, "successfully updated with localized meta information.")
  return(out)  
}




#' Method to assign attributes of class metaFixed
#' 
#' @author Matthias Bannert
#' @param x any R object to assign meta data to, typically a zoo object.
setGeneric("metaFixed<-", 
           def= function(x, value) standardGeneric("metaFixed<-"),
           useAsDefault= function(x, value){
             if (!is(value,"metaFixed")) 
               stop("trying to set attribute metaFixed incorrectly.") 
             attr(x, "metaFixed") <- value
             x
           })

#' Method to assign attributes of class metaLocalized
#' 
#' @author Matthias Bannert
#' @param x any R object to assign meta data to, typically a zoo object.
setGeneric("metaLocalized<-", 
           def= function(x, value) standardGeneric("metaLocalized<-"),
           useAsDefault= function(x, value){
             if (!is(value,"metaLocalized")) 
               stop("trying to set attribute metaLocalized incorrectly.") 
             attr(x, "metaLocalized") <- value
             x
           })
