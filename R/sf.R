
# POINT
# LINESTRING
# POLYGON
# MULTIPOINT
# MULTILINESTRING
# MULTIPOLYGON

print.sf <- function(x, ...){
    cat("simple features ",attr(x,"type")," object\n")
    cat("dimension: ",attr(x,"dimension"),"\n")
    cat("measured: ",attr(x,"measured"),"\n")
    ## NextMethod()
}

c.sf <- function(..., recursive=FALSE){
    stop("don't make longer vectors of simple features objects")
    list(...)
}


### bounds of sf object

bounds <- function(x, ...){
    UseMethod("bounds")
}

bounds.POINT <- function(x, ...){
    c(xmin=x[1], xmax=x[1], ymin=x[2], ymax=x[2])
}

bounds.POLYGON <- function(x, ...){
    ## first ring must be outside
    xr=range(x[[1]][,1])
    yr=range(x[[1]][,2])
    c(xmin=xr[1], xmax=xr[2], ymin=yr[1], ymax=yr[2])
}

bounds.MULTIPOLYGON <- function(x, ...){
    ## bounds of all first rings of all top-level elements
    bb = apply(sapply(x, function(r){apply(r[[1]],2,range)}),2,range)
    c(xmin=bb[1,1], xmax=bb[2,1], ymin=bb[1,2], ymax=bb[2,2])
}

### drawing sf objects

draw <- function(sfo, ...){
    UseMethod("draw")
}

draw.sf <- function(sfo, ...){
    NextMethod()
}

draw.POINT <- function(sfo, pointstyle, ...){
    do.call("points",makePointStyle(list(x=sfo[1],y=sfo[2],...)))
}

draw.POLYGON <- function(sfo, linestyle, fillstyle, ...){
    polygon(do.call("rbind",sfo),...,border=NA, col="red")
    for(i in 1:length(sfo)){
        lines(sfo[[i]],...)
    }
}

draw.LINESTRING <- function(sfo, linestyle, ...){
    lines(sfo,...)
}

draw.MULTIPOLYGON <- function(sfo, linestyle, fillstyle, ...){
    for(p in sfo){
        polygon(do.call("rbind",p),...,border=NA, col="red")
    }
    for(p in sfo){
        for(r in p){
            lines(r, ...)
        }
    }
}
