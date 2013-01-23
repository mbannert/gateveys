# some vector that contains NAs
someVector <- c(1,2,NA,NA,3,1,2,2,0,9,4,NA,9,2)

# replace all NAs by string and turn 
# variable into factor
replaceNA(someVector,"empty")

# turning NA into a category is common use case
table(someVector)
table(someVector,useNA="alw")
table(replaceNA(someVector,"empty"))