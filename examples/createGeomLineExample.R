# create a geom first, i.e. basically a layer that contains
# a single time series line
g <- createGeomLine()
# add it to a ggplot object and add lots of other options
g1 <- ggplot() + g + xlab("") + ylab("")
g1

# plotTs is a convenience plotting function based on
# createGeomLine
plotTs(sts1, # first time series
       sts2, # second time series
       sts3, # third time series  
)

