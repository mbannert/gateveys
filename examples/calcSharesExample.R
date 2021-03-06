# draw some weights from a mixed distribution
w <- drawFromMixed(1000,list(rnorm=list(mean=1000,sd=250),
                             rexp=list(),
                             rchisq=list(df=10)),
                             c(0.1,0.5,0.4))
w <- ceiling(unlist(w))
# create a basic categorical samle panel
sData <- generateSamplePanel(80,3,"q",c(1995,1),c(2001,3),weight=w)
# turn it into data.table to make use of indexing !
sData[,grep("quest",names(sData))] <- lapply(sData[,grep("quest",names(sData))],as.factor)

# add a NAs and group to the mix
sData <- generateRandomNAs(sData,c(1:3,7),30,1)
sData <- merge(sData,generateRandomGroups(3,unique(sData$uid)),by="uid")

# set some size class, only needed for the weighByMultiClasses function
sData <- setSizeClass(sData,thresholdList=list(M=20,L=200),size="weight",
             sectorColumn="all")

# turn data.frame to data.table to use the keys
sData.dt <- data.table(sData,key=c("year","period","group"))
# run calcShares
calcShares(sData.dt,"question_1","weight")

# grep questions
questions <- grep("^quest",names(sData),value=TRUE)

# apply weighByNOGA over all questions
listOfResults <- weighByMultiClasses(sData.dt,"group",questions,"weight")
names(listOfResults) <- "group"
# note that if group is character vector > 1 this lists contains multiple 
# elements each representing one class (aggregation level)

