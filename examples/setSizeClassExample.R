# generate survey likerandom sample
srvyData <- generateSamplePanel(500,3,"quarterly",
                    c(1999,1),
                    c(1999,1),
                    weight=round(runif(500,2,80)))

# add a sector, cause function supports different 
# thresholdLists for different sectors
srvyData$sector <- "A"

# create classes based on weight
srvyData <- setSizeClass(srvyData,
                              thresholdList=list(M=20,L=40),
                              sizeColumn="weight", 
                              sectorColumn="sector")
