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
#' @param addKey additional time series key for second referencing system, 'fameKey' 
#' is the historic default referring to SUNGUARD's time series system FAME. 
#' @example examples/dataframe2tsExample.R
dataframe2ts <- function(dataFrame,frequency="Y",period="",year="year",
                         var="share",env=.GlobalEnv,addKey="fameKey"){
  # make sure data.tables are turned to data.frames
  dataFrame <- as.data.frame(dataFrame)
  if(!is.na(pmatch(tolower(frequency),"quarterly"))){
    idx <- as.yearqtr(paste(dataFrame[,year],"Q",dataFrame[,period],sep=""))
    idx <- as.Date(idx)
  }
  if(exists("zoo")){
    res <- zoo(dataFrame[,var],idx)
    attr(res,'frequency') <- frequency
    if(!is.null(attr(dataFrame, addKey))){
      attr(res,addKey) <- attr(dataFrame,addKey)
    }
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

#' Swap object Name based on two attribute names
#' 
#' This function swaps the name of an R object between two of its attributes. 
#' Objects need to have at least to character objects to use this function. 
#' Typically one character object should be equal to the objects original name
#' in order to be able to swap back to the old name. 
#' 
#' @author Matthias Bannert
#' @param x R object or character name of an object
#' @param attr1 name of the first attribute. Default is 'tskey'. Typically the 
#' name of this attribute is equal to the initial object name
#' @param attr2 name of the second attribute. Object will change its name to the
#' value of this second attribute. Default value is 'fameKey'
swapObjectName <- function(x,attr1="tskey",attr2="fameKey"){
  
  if(is.character(x)){
    objName <- x
    x <- get(x)
  } else {
    objName <- deparse(substitute(x))
  } 
  
  n1 <- attr(x,attr1)
  n2 <- attr(x,attr2)
  
  if(objName == n1){
    assign(n2,x,envir=.GlobalEnv)
    
  } else {
    assign(n1,x,envir=.GlobalEnv)
  }
  rm(list=c(objName),envir=.GlobalEnv)
  paste("Object name swapped.")
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
#' @param keepKey logical default FALSE, keep key outside the class. 
#' setting this argument to TRUE can help to swap names based on keys..
#' @seealso \code{\link{addLocalizedMetaData}}
#' @example examples/addMetaDataExample.R
addFixedMetaData <- function(x,stat="normal",
                                   unit="percent",
                                   unitMulti=1,qType="qualitative",
                                   env=.GlobalEnv,
                                   keepKey=F){
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
  # remove that duplicate information, but keep key on demand... 
  if(!keepKey){
    attr(x,"tskey") <- NULL
  }
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
#' @param wInfo character string that contains some weighting information,
#' typically 'weighted', 'unweighted' or ''.
#' @param qDict a data.frame that contains some meta descriptions for 
#' questions. Might be read from an external source.
#' @param env environment to assign time series too, .GlobalEnv is default.
#' @param desc language specific description
#' @seealso \code{\link{addFixedMetaData}}
#' @example examples/addMetaDataExample.R
addLocalizedMetaData <- function(x,attrName = "de",qDict=questionDescription.de,
                                 aDict=DLU_ANTW,
                                 wInfo="Gewichtung mit Beschäftigten (capped)",
                                 desc="Dienstleistungsumfrage basierend auf NOGA08",
                                 env=.GlobalEnv){
  # make this function able to handle 
  # both names and objects themselves
  if(is.character(x)){
    objName <- x
    x <- get(x)
  } else {
    objName <- deparse(substitute(x))  
  } 
  
  
  # localize this function 
  if(attrName=="de") lang <- "D"
  if(attrName=="fr") lang <- "F"
  if(attrName=="it") lang  <- "I"
  
  
  # split name into its informative parts
  chunks <- strsplit(objName, "\\.")[[1]]
  srvy <- chunks[3]
  aLvl <- chunks[4]
  q <- str_extract(chunks[5],"[0-9]{1,4}")
  nclass <- chunks[6]
  itm <- str_extract(chunks[length(chunks)],"[0-9]{1}")
  
  # get selected item, all items
  s.itm <- aDict[aDict[,"SPRACHE"] == lang & 
                   aDict[,"CODE"] == itm &
                   aDict[,"FRAGENR"] == q, "BEDEUTUNG"]
  
  a.itm <- paste(aDict[aDict[,"SPRACHE"] == lang & 
                         aDict[,"FRAGENR"] == q, "BEDEUTUNG"],collapse=", ")
  
  # create new Class metaLocalized
  m <- new("metaLocalized",
           title = qDict[qDict$qkey == chunks[5],"questionShortLabel"],
           selectedItem = s.itm,
           description = desc,
           aLevel = aLvl,
           selectedGroup = nclass, 
           survey = SURVEY,
           questionWording = as.character(qDict[qDict$qkey == chunks[5],
                                                "questionWording"]),
           itemLevels = a.itm,
           weightingInformation = wInfo
  )
  
  attr(x, paste("metaLocalized", attrName, sep = ".")) <- m
  assign(objName, x, envir = env)
  out <- paste(objName, " successfully updated with localized meta information.")
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
