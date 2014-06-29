
##' geometry class object
##'
##' geometry class
##' @title create a geom class
##' @param x a vector
##' @return a geom vector
##' @author Barry Rowlingson
##' @export
geom <- function(x){
    class(x)="geom"
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
    
    cl = oldClass(x)
    class(x) = NULL
    val = NextMethod("[")
    class(val) = cl
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
