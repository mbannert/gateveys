#' Create geoms of type line to plot time series with ggplot
#' 
#' This function creates geoms for ggplot2. Time series with metainformation 
#' can be used as arguments to this function in order to create plots with 
#' metainformation. Though it can be used separately for more custom plot, 
#' createGeomLine is typically used with the convenience plotting function
#' plotTs. 
#' 
#' @author Matthias Bannert
#' @param sts zoo survey time series object with attribute metaLocale.
#' @param lang character string indicating which language will be used in the labels. 
#' @return geom a ggplot layer containing a line for use in a chart. 
#' @seealso \code{\link{plotTs}}
createGeomLine <- function(sts,lang="de_CH"){
  metaLocale <- paste("metaLocalized",lang,sep=".") 
  storeAttr <- attributes(sts)
  dframe <- as.data.frame(sts)
  nm <- paste(storeAttr[[metaLocale]]@survey,
              paste(storeAttr[[metaLocale]]@title,
                    storeAttr[[metaLocale]]@selectedItem,sep=", "),              
              sep=" \n")
  idx <- attr(sts,'index')
  d <- cbind(idx,dframe,nm)
  names(d) <- c("time","value","description")
  g <- geom_line(data=d,aes(time,value,colour = description),size=1.5)
  return(g)
}

#' plot (multiple) time series conveniently
#' 
#' This function plots one or more time series into the same plot. It uses 
#' createGeomLine to create multiple layers of line geoms. Just input several 
#' time series objects separated by commas to this function to obtan all 
#' series in one plot.
#' 
#' @author Matthias Banert
#' @param ... vector of time series objects
#' @param pal character string containing color palette
#' @param lg logical paramater that indicates whether to use a language specfic
#' legend
#' @param llabel character string to influence title of the legend
#' @param lang character string that indicates which language should be use in 
#' the meta description. 
#' @seealso \code{\link{createGeomLine}}
plotTs <- function(...,pal=eth.colors(),lg=T,llabel="Legende",lang="de_CH"){
  dots <- list(...)
  l <- length(dots)
  geoms <- lapply(dots,function(x) createGeomLine(x,lang))
  pal <- unique(pal)
  if (lg == T){
    lgnd <- scale_colour_manual(llabel,values = pal)  
  }
  p <- ggplot() + geoms + lgnd +theme_bw() +xlab("") +ylab("") +
    theme(panel.grid.major.y = element_line(size = 0.1,
                                            colour = '#333333'))+
    theme(aspect.ratio = 2/(1+sqrt(5))) + 
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12))
  return(p)
}

#'  ETH color palette
#'  
#'  Color palette used by the ETH Zurich according to its CD manual. 
#'  
#'  @author Markus Graf
#'  @param n numeric variable indicates the number of colors taken from the
#'  palette. Maximum is 9 (default).
#'  @return character vector containg RGB HEX 'equivalents' of ETH Zurich
#'  Pantone colors.
#'  @seealso \code{\link{palette},\link{colors},\link{terrain.colors},\link{gray}}
eth.colors <- function (n = 9) {
  return(c(Pantone286 = "#0055FA", Pantone313 = "#00CCDE", 
           Pantone3272 = "#00FF8F", Pantone362 = "#45e800", Pantone390 = "#b6ea00", 
           Pantone130 = "#ffb200", Pantone032 = "#ff0000", Pantone199 = "#ff0060", 
           Pantone463 = "#a04600")[1:n])
}
