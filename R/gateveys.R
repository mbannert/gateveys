#' Apply an exponent function if value beyond max threshold
#' 
#' This function adjusts the size of values in a numeric vector once value go
#' beyond a certain threshold. The adjustment is done by an exponential 
#' function with the default exponent 0.7. In the context of business tendency
#' surveys this function is often used to lower the influence of very large
#' companies when employment based weighting is used. 
#'  
#' @author Matthias Bannert
#' @param weight input numeric vector
#' @param threshold input numeric giving the threshold beyond which exp function should be applied
#' @param exponent input numeric giving the exponent, typically below one as influence of big weights should be declined
#' @return numeric vector of capped weights
#' @example examples/capWeightExample.R
#' @seealso \code{\link{setSizeClass}}
capWeight <- function (weight, threshold = 500, exponent = 0.7) 
{
  runCapWeight <- function(n,threshold,exponent){
    ret <- n
    if (n > threshold) 
      ret <- threshold + (n - threshold)^exponent
    return(ret)
  }
  # vectorize this function
  res <- sapply(weight,runCapWeight,threshold=threshold,exponent=exponent)
  return(res)
}

#' Replace NA in categorical data
#' 
#' This functions replaces NA by the string 
#' subst. The function can easily be used within lapply strutctures.
#' 
#' @author Matthias Bannert
#' @param x vector that possibly contains NAs
#' @param subst character string that contains substitution of NA.
#' @example examples/replaceNAExample.R
#' @return factor that contains a level denoted by 'subst' instead of NAs.
replaceNA <- function(x,subst){
  x <- as.character(x)
  x[is.na(x)]  <- subst
  x <- as.factor(x)
  return(x)
}

#' Add Size Class based on weights
#' 
#' @author Matthias Bannert
#' @param x data.frame that contains the dataset
#' @param thresholdList List of group specific threshold assignments
#' @param sector
#' @param sizeColumn
#' @param sectorColumn
#' @param resultColumn
#' @param minimalClass
#' @return factor containing size classes defined in thresholdList 
#' and minimalClass. e.g.: S,M,L
#' @example examples/setSizeClassExample.R
#' @seealso \code{\link{setSizeClass}, \link{generateSamplePanel}}
setSizeClass <- function (x, thresholdList, sector = "all", sizeColumn = "size", 
                          sectorColumn = "sector", resultColumn = "sizeClass", minimalClass = "S") 
{
  stopifnot(is.data.frame(x))
  stopifnot(is.list(thresholdList))
  stopifnot(sizeColumn %in% names(x))
  stopifnot(sectorColumn %in% names(x))
  if (!(sizeColumn %in% names(x))) 
    x[, resultColumn] <- NA
  if (sector == "all") {
    x[, resultColumn] <- minimalClass
    for (i in 1:length(thresholdList)) {
      x[x[, sizeColumn] > thresholdList[[i]], resultColumn] <- names(thresholdList)[i]
    }
  }
  else {
    x[x[, sectorColumn] == sector, resultColumn] <- minimalClass
    for (i in 1:length(thresholdList)) {
      x[x[, sectorColumn] == sector & x[, sizeColumn] > 
          thresholdList[[i]], resultColumn] <- names(thresholdList)[i]
    }
  }
  return(x)
}



#' calculate the weighted share that corresponds to a particular category
#' 
#' @author Matthias Bannert
#' @param dt input data.table with fixed keys
#' @param var input categorical (factor) variable of interest, is basically an additional varying key
#' @param sizeColumn input numeric vector denoting the weight, in BTS typically number of employees
#' @return data.table containing shares according to fixed and variable keys
#' @example examples/calcSharesExample.R
#' @seealso \code{\link{weighByMultiClasses},\link{data.table}} 
calcShares <- function (dt, var, sizeColumn) 
{
  v <- as.name(var)
  sizeColumn <- as.name(sizeColumn)
  dt[i = !(is.na(eval(v))), j = {
    n <- sum(.SD[, eval(sizeColumn)])
    .SD[, list(sumTest = sum(eval(sizeColumn)), sumTestTotal = n, 
               AN = length(.SD[, eval(sizeColumn)]), share = sum(eval(sizeColumn))/n), 
        by = eval(as.character(v))]
  }, by = key(dt)]
}




#' Iterate calcShares over multiple groups
#' 
#' @author Matthias Bannert
#' @param dt input data.table containing id,group and participant answer information
#' @param keyvector vector containing the categories to group by
#' @param variables vector of categorical variable, typically questions in survey
#' @param weight vector containing quantitative weights, typically number of employees
#' @return list of lists that contains results with groups
#' on the toplevel and variables on the level velow
#' @example examples/calcSharesExample.R 
#' @seealso \code{\link{capWeight},\link{calcShares},\link{burstList}}
weighByMultiClasses <- function (dtable, keyvector, variables, weight) 
{
  stopifnot(is.data.table(dtable))
  stopifnot(is.vector(keyvector))
  stopifnot(is.vector(variables))
  fxdkey <- key(dtable)
  resultList <- list()
  for (i in 1:length(keyvector)) {
    dtable <- dtable[!(is.na(dtable[, get(keyvector[i])])), 
                     ]
    keys <- c(keyvector[i], fxdkey)
    setkeyv(dtable, keys)
    if (exists("mclapply")) {
      resultList[[i]] <- mclapply(variables, FUN = function(X) {
        do.call(calcShares, list(dtable, X, weight))
      })
    }
    else {
      resultList[[i]] <- lapply(variables, FUN = function(X) {
        do.call(calcShares, list(dtable, X, weight))
      })
    }
    names(resultList[[i]]) <- variables
  }
  names(resultList) <- keyvector
  return(resultList)
}
#' draw random variables from a mixed distribution
#' 
#' @author Matthias Bannert
#' @param n number of total draws from all distributions
#' @param li named list of random distribution functions that contains 
#' vectors of with corresponding function parameters
#' @param probs vector of drawing probabilities from different functions
#' which has to be the same length as list
#' @return vec vector of random variables 
#' @example examples/drawFromMixedExample.R
drawFromMixed <- function(n,li,p){
  stopifnot(length(li) == length(p))
  stopifnot(sum(p) == 1)
  distrShare <- sample(1:length(li),n,replace=T,prob=p)
  x <- table(distrShare)
  li <- mapply(c,li,x)
  res <- lapply(names(li),function(X) do.call(X,args=li[[X]]))
  names(res) <- names(li)
  return(res)
}

#' generate sample panel
#'
#' @author Matthias Bannert
#' @param n number of observations
#' @param q number of questions
#' @param freq character string indicating frequency,
#' can be "annually", "quarterly", "monthly". Supports fuzzy unabmigous strings.
#' @param start vector containing year,period
#' @param end vector containing year,period'
#' @param seed for reproducible random number generation
#' @param rng of the categorical scale default 1-3 
#' @param weight vector of weights, if of greater length than 
#' n, a sample of length n is drawn, default is set to 1 (equal weights)
#' @return data.frame that represents BTS typical data
#' @example examples/generateRandomExample.R
generateSamplePanel <- function(n,q,freq,start,end,seed=1,
                                rng=1:3,groupList=NA,weight=1){
  set.seed(seed)
  uid <- 1:n
  if(!is.na(pmatch(freq,"annually"))){
    plength <- (end-start)+1
    uid <- rep(uid,plength)
    year <- rep(start:end,each=n)
    df <- cbind(uid,year)
    qmat <- as.data.frame(matrix(round(runif(n*q,
                                             min(rng),
                                             max(rng))),
                                 nrow=n,ncol=q))
    df <- cbind(df,qmat)
    names(df)[-c(1,2)] <- paste("question",1:q,sep="_")   
  } else if(!is.na(pmatch(freq,"quarterly"))){
      stopifnot(length(start) == 2)
      stopifnot(length(end) == 2)
      plength <- ((end[1]-start[1])*4)+(end[2]-start[2])
      gr1 <- expand.grid(1:n,
                        start[1]:end[1],
                        1:4)
      o <- order(gr1$Var2,gr1$Var3,gr1$Var1)
      gr2 <- gr1[o,]
      row.names(gr2) <- 1:nrow(gr2)
      fl <- as.numeric(row.names(gr2[gr2$Var2 == start[1] &
                                       gr2$Var3 == start[2],][1,]))
      ll <- as.numeric(row.names(gr2[gr2$Var2 == end[1] &
                                       gr2$Var3 == end[2],][n,]))
      gr2 <- gr2[fl:ll,]
      qmat <- as.data.frame(matrix(round(runif(nrow(gr2)*q,
                                               min(rng),
                                               max(rng))),
                                   nrow=nrow(gr2),ncol=q))
      df <- cbind(gr2,qmat)
      names(df) <- c("uid","year","period",paste("question",1:q,sep="_"))
  } else if(!is.na(pmatch(freq,"monthly"))){
    stopifnot(length(start) == 2)
    stopifnot(length(end) == 2)
    gr1 <- expand.grid(1:n,
                       start[1]:end[1],
                       1:12)
    o <- order(gr1$Var2,gr1$Var3,gr1$Var1)
    gr2 <- gr1[o,]
    row.names(gr2) <- 1:nrow(gr2)
    fl <- as.numeric(row.names(gr2[gr2$Var2 == start[1] &
                                     gr2$Var3 == start[2],][1,]))
    ll <- as.numeric(row.names(gr2[gr2$Var2 == end[1] &
                                     gr2$Var3 == end[2],][n,]))
    gr2 <- gr2[fl:ll,]
    qmat <- as.data.frame(matrix(round(runif(nrow(gr2)*q,
                                             min(rng),
                                             max(rng))),
                                 nrow=nrow(gr2),ncol=q))
    df <- cbind(gr2,qmat)
    names(df) <- c("uid","year","period",paste("question",1:q,sep="_"))    
  }
  if(!is.na(groupList)){
    
  }
  if(length(weight) != 1){
    wtable <- as.data.frame(cbind(uid,sample(weight,n,replace=T)))
    names(wtable) <- c("uid","weight")
    df <- merge(df,wtable,by="uid")
  } else {
    df$weight <- weight
  }
  return(df)  
}

#' generateRandomGroups
#' 
#' @author Matthias Bannert
#' @param n number of groups, ranges from 1-26 as groups are denoted by letters
#' @param uid vector of uid to assign random groups to
#' @return data.frame that can be merged with a dataset
#' @seealso \code{\link{generateSamplePanel}}
#' @example examples/generateRandomExample.R
generateRandomGroups <- function(n,uid,variables=LETTERS){
  grps <- variables[1:n]
  df <- data.frame(group=sample(grps,length(uid),replace=T),
                   uid = uid)
  return(df)
}


#' add random NAs to a data.frame
#' 
#' @author Matthias Bannert
#' @param df input
#' @param protected are cols of the data.frames which need to be proteced from random NAs
#' @param number of NAs that should be introduced. 
#' @param seed for reproducible random number generation
#' @return data.frame that contains several random NAs within its questions
#' @example examples/generateRandomExample.R 
generateRandomNAs <- function(df,protected,numberOfNAs,seed){
  if(is.numeric(protected)){
    qmat <- as.matrix(df[,-protected])
  } else {
    qmat <- as.matrix(df[,match(protected,names(df))])
  }
  set.seed(seed)
  n <- nrow(qmat)
  q <- ncol(qmat)
  stopifnot((n*q) > numberOfNAs)
  positions <- 1:(n*q)
  draw <- sample(positions,numberOfNAs,replace=F)
  qmat[draw] <- NA
  qmat <- as.data.frame(qmat)
  res <- cbind(df[,protected],qmat)
  return(res)
}

#' split list of results by factor
#' 
#' @author Matthias Bannert
#' @param li list that should be splitted, typically a list of results generated by weighByMultiClasses
#' @param fac a factor to split by
#' @return a list of data.frames obtained by splitting
#' @seealso \code{\link{burstList}}
splitList <- function(li,fac){
  res <- lapply(li,function(x) split.data.frame(x,f=getElement(x,fac)))
  return(res)
}

#' burst a list into data.frames representing univariate time series
#' 
#' @author Matthias Bannert
#' @param resultList list of results generated by weighByMultiClasses
#' @return list of data.frames, data.frames contain exactly one variable
#' @seealso \code{\link{calcShares},\link{weighByMultiClasses},
#' \link{splitList}}
#' @example examples/burstListExample.R
burstList <- function(resultList){
  n <- names(resultList)
  res <- lapply(n,function(x) splitList(resultList[[x]],fac=x))
  return(res)
}

#' linearize nested lists
#' 
#' This function un-nests a list and stores the result into a named list with 
#' all elements on the first depth level. The function is recursive and its 
#' input is only limited by the maximum nesting level R allows for. 
#' 
#' @author Akhil Behl <akhilsbehl@@gmail.com>, Matthias Bannert
#' <bannert@@kof.ethz.ch>
#' @param nList input nested List
#' @param linearizeDataFrames logical to indicate whether data.frames should be linearized too
#' @param nameSep character to seperate different list elements
#' @param forceNames logical to indicate whether original names should be removed and
#' sequential numbers should be used
#' @return a linear list whose names hold the information containing in the 
#' nesting of the input list
#' @references This function was originally created by Akhil Behl and was
#' only slightly modified by the author of this package.
#' The original code can be found on his website: 
#' https://sites.google.com/site/akhilsbehl/geekspace/articles/r/linearize_nested_lists_in_r
#' @example examples/linearizeNestedListExample.R
linearizeNestedList <- function (nList, linearizeDataFrames=FALSE,
                                 nameSep=".", forceNames=FALSE) {
  # some checks
  stopifnot(is.character(nameSep), length(nameSep) == 1)
  stopifnot(is.logical(linearizeDataFrames), length(linearizeDataFrames) == 1)
  stopifnot(is.logical(forceNames), length(forceNames) == 1)
  if (! is.list(nList)) return(nList)
  #
  # If no names or forNames is TRUE on the top-level list coerce names.
  # Recursion shall handle naming at all levels.
  #
  if (is.null(names(nList)) | forceNames == TRUE)
    names(nList) <- as.character(1:length(nList))
  # 
  # What to do if object is simply a data.frame 
  # a) just return it 
  #
  if (is.data.frame(nList) & linearizeDataFrames == FALSE)
    return(nList)
  #
  # b) store it in list format, so it gets linearized 
  # like a list
  #
  if (is.data.frame(nList) & linearizeDataFrames == TRUE)
    return(as.list(nList))
  #
  # Book-keeping code to employ a while loop.
  #
  A <- 1
  B <- length(nList)
  #
  # We use a while loop to deal with the fact that the length of the nested
  # list grows dynamically in the process of linearization.
  #
  while (A <= B) {
    element <- nList[[A]]
    eName <- names(nList)[A]
    if (is.list(element)) {
      #
      # Before and After to keep track of the status of the top-level trunk
      # below and above the current element.
      #
      if (A == 1) {
        before <- NULL
      } else {
        before <- nList[1:(A - 1)]
      }
      if (A == B) {
        after <- NULL
      } else {
        after <- nList[(A + 1):B]
      }
      #
      # data.frame handling
      #
      if (is.data.frame(element)) {
        if (linearizeDataFrames == TRUE) {
          #
          # `jump` takes care of how much the list shall grow in this step.
          #
          jump <- length(element)
          nList[[A]] <- NULL
          #
          # Generate or coerce names as need be.
          #
          if (is.null(names(element)) | forceNames == TRUE)
            names(element) <- as.character(1:length(element))
          #
          # Just throw back as list since dataframes have no nesting.
          #
          element <- as.list(element)
          #
          # Update names
          #
          names(Element) <- paste(eName, names(element), sep=nameSep)
          #
          # Plug the branch back into the top-level trunk.
          #
          nList <- c(before, element, after)
        }
        jump <- 1
        #
        # else = it's not a data.frame!
        #
      } else {
        nList[[A]] <- NULL
        #
        # Go recursive! :)
        #
        if (is.null(names(element)) | forceNames == TRUE)
          names(element) <- as.character(1:length(element))
        element <- linearizeNestedList(element, linearizeDataFrames,
                                       nameSep, forceNames)
        names(element) <- paste(eName, names(element), sep=nameSep)
        jump <- length(element)
        nList <- c(before, element, after)
      }
      #
      # else = element is not a list anymore 
      #
    } else {
      jump <- 1
    }
    #
    # Update book-keeping variables.
    #
    A <- A + jump
    B <- length(nList)
  }
  return(nList)
}


#' Add tskey attribute to every data.frame in a list of data.frames
#' 
#' This function appends an attribute named tskey to every data.frame in 
#' a given named list of data.frames. List names are used as
#' tskey values. This small function could also be integrated to 
#' the big linearize function
#' 
#' @author Matthias Bannert
#' @param li named list of data.frames
#' @example examples/linearizeNestedListExample.R
for.setattr <- function(li){
  stopifnot(!is.null(names(li)))
  for (i in seq_along(li)) 
    setattr(li[[i]], name = 'tskey', value = names(li[i]))
}
  
#' Turn Names Generated from the Aggregation Process into valid tskeys
#' 
#' This function is yet quite specific and works only with a list of 
#' timeseries storen in multiple separate date.frames. Currently the 
#' function has no example yet. 
#' 
#' @author Matthias Bannert
#' @param nms character vector that typically contains the names of a
#'  list of data.frames
#'  @param country character country code
#'  @param provider character string to identify data provider in a country
#'  @param survey character string to identify the data
#'  @return character string containing a valid gateveys tskey
#'  @seealso \code{\link{for.setattr},\link{linearizeNestedList}}
checkKeys <- function(nms,country,provider,survey,li){
  nms <- names(li)
  rootChunk <- unlist(strsplit(nms,"\\.[0-9]"))
  items <- regexec("\\.[0-9]",nms)
  matches <- regmatches(nms,items)
  n <- nchar(matches)[1]
  items <- substr(unlist(matches),n,n)
  keys <- paste(paste(country,provider,survey,rootChunk,sep="."),
                ".item_",items,sep="")
  return(keys)
}

#' Count NAs 
#' 
#' This function counts NAs. It is designed to find NAs per column in survey based data. 
#' Best used when used with ldply. 
#' 
#' @author Matthias Bannert
#' @param x vector that potentially contains NAs
#' @return y line named row vector
#' @example examples/countNAsExample.R
countNAs <- function(x){
  y <- table(is.na(x))
  n <- as.character(deparse(substitute(x)))
  n <- strsplit(n,"\\$")[[1]][2]
  stopifnot(length(y) %in% c(1,2))
  if(length(y) == 1) {    
    nm <- names(y)
    y <- c(y,0)
    if (nm == "FALSE") {
      names(y) <- c("FALSE","TRUE")
      y <- t(y)
    }
    else {names(y) <- c("TRUE","FALSE")
          y <- t(as.matrix(y))
          # make sure that FALSE is always first
          # this is important if you ldply this function!!
          y <- t(y[,c(2,1)])
    }
    rownames(y) <- n
    y <- cbind(y,sum(y))    
    colnames(y)[3] <- "Total"
    return(y)
  }
  if(length(y) == 2){        
    y <- t(as.matrix(y))
    y <- cbind(y,sum(y))
    rownames(y) <- n
    colnames(y)[3] <- "Total"
    y  
  }  
}






