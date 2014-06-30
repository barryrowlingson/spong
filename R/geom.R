
##' geometry class object
##'
##' geometry class
##' @title create a geom class
##' @param x a vector
##' @return a geom vector
##' @author Barry Rowlingson
##' @export
geom <- function(x, crs=NA){
    class(x)="geom"
    attr(x,"crs")=crs
    return(x)
}

print.geom = function(x,...){
    if("crs" %in% names(attributes(x))){
        crs = attr(x,"crs")
    }else{
        crs="None"
    }
    print(paste0(substr(x,1,10),"..."))
    cat("CRS:" ,crs,"\n")
    
}

"[.geom" = function (x, ..., drop = TRUE) 
{
    
    xcrs=crs(x)
    cl = oldClass(x)
    class(x) = NULL
    val = NextMethod("[")
    class(val) = cl
    crs(val)=xcrs
    val
}

as.character.geom = function(x,...){
  paste0(substr(x,1,10),"...")
}

format.geom = function(x,...){
  as.character.geom(x)
}

crs <- function(x){
    NextMethod("crs")
}

crs.geom <- function(x){
    if("crs" %in% names(attributes(x))){
        return(attr(x,"crs"))
    }
    return("")
}

"crs<-" <- function(x,value){
    NextMethod("crs<-")
}

"crs<-.geom" <- function(x,value){
    attr(x,"crs")=value
    x
}

geom_from_points <- function(spp){
    g = geom(writeWKT(spp,byid=TRUE), crs=proj4string(spp))
    g
}

as.data.frame.geom <- function(x, row.names=NULL, optional=FALSE,...,nm=paste(deparse(substitute(x),width.cutoff=500L),collapse=" ")){
    value = list(x)
    attr(value,"row.names") <- seq_along(x)
    class(value)="data.frame"
    value
}

plot.geom <- function(x,...){
    pts = lapply(x, readWKT, p4s=crs(x))
    plot(do.call(rbind,pts))
}
