dframe <- data.frame(test_vector_1 = c(1,2,324,5,1,2),
              test_vector_2 = c(NA,324,NA,1,NA,NA),
                     test_vector_3=c(NA,NA,NA,NA,NA,NA))

# single calls
countNAs(dframe$test_vector_1)  
countNAs(dframe$test_vector_2)  
countNAs(dframe$test_vector_3)  

# if plyr is loaded, try
# require(plyr)
# ldply(dframe,countNAs)
# can easily rename that because
# FALSE is always first with countNAs



