    
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

