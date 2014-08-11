
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

bounds <- function(sfo, ...){
    UseMethod("bounds")
}

bounds.POINT <- function(sfo, ...){
    data.frame(xmin=sfo[1], xmax=sfo[1], ymin=sfo[2], ymax=sfo[2])
}

bounds.POLYGON <- function(sfo, ...){
    ## first ring must be outside
    xr=range(sfo[[1]][,1])
    yr=range(sfo[[1]][,2])
    data.frame(xmin=xr[1], xmax=xr[2], ymin=yr[1], ymax=yr[2])
}

bounds.MULTIPOLYGON <- function(sfo, ...){
    ## bounds of all first rings of all top-level elements
    bb = apply(sapply(sfo, function(r){apply(r[[1]],2,range)}),2,range)
    data.frame(xmin=bb[1,1], xmax=bb[2,1], ymin=bb[1,2], ymax=bb[2,2])
}

### drawing sf objects

draw <- function(sfo, ...){
    UseMethod("draw")
}

draw.sf <- function(sfo, ...){
    cat("plot...\n")
    NextMethod()
}

draw.POINT <- function(sfo, ...){
    cat("point\n")
}

draw.POLYGON <- function(sfo, ...){
    cat("polygon...\n")
}

draw.LINESTRING <- function(sfo, ...){
    cat("line string\n")
}

