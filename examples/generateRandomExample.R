# generate survey likerandom sample
srvyData <- generateSamplePanel(500,3,"quarterly",
                                c(1999,1),
                                c(1999,1),
                                weight=round(runif(500,2,80)))


# generate 
grps <- generateRandomGroups(3,1:500,LETTERS)

# merge group information by uid
srvyWgroups <- merge(grps,srvyData,by="uid")

# add some NAs (to simulate item non-response)
# check which cols should be protected from 
# being hit by random NAs
head(srvyWgroups)
randomPanel <- generateRandomNAs(srvyWgroups,c(1,2,3,4),50,1)
randomPanel