

plot.spong = function(x,...){
  d = as.SpatialPolygonsDataFrame.spong(x)
  plot(d,...)
}

print.spong <- function(x,...){
    geom = attr(x,"the_geom")
    crs = crs(x[[geom]])
    NextMethod(print)
    cat("geometry column: ",geom,"\n")
    cat("crs: ",crs,"\n")
    
}

"the_geom<-" <- function(d,value){
    name=value
    if(!name %in% names(d)){
        stop("no such column ",name)
    }
    attr(d,"the_geom")=name
    class(d)=c("spong","data.frame")
    d
}

setgeom <- function(d, value){
    the_geom(d)=value
    d
}

