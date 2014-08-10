
##' geometry class object
##'
##' geometry class
##' @title create a geom class
##' @param x a list of simple features (sf) class objects
##' @return a geom vector
##' @author Barry Rowlingson
##' @export
geom <- function(x, crs=NA){
    for(ob in x){
        if(!inherits(ob,"sf")){
            stop("non-simple features object in geom constructor")
        }
    }
    class(x)="geom"
    attr(x,"crs")=crs
    return(x)
}

geom1 <- function(g,...){
    geom(list(g),...)
}

print.geom = function(x,...){
    if("crs" %in% names(attributes(x))){
        crs = attr(x,"crs")
    }else{
        crs="None"
    }
    print(format(x,...))
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

#c.geom <- function(..., recursive=FALSE){
#structure(c(unlist(lapply(list(...), unclass))), class = "geom")
#}

c.geom <- function(...,recursive=FALSE){
###
### make a vectype - check all args are vectypes
###
    llply(list(...),function(ob){
        if(!inherits(ob,"geom")){
            stop("Can't join non-geoms to a geom")
        }
    })
    obs = NextMethod()
    class(obs) <- "geom"
    obs
}


as.character.geom = function(x,...){
    ## must return same length as x
    return(sapply(x,function(ob){attr(ob,"type")}))
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

as.data.frame.geom <- function (x, row.names = NULL,
                                optional = FALSE, ...,
                                nm = paste(
                                    deparse(substitute(x),width.cutoff = 500L),
                                    collapse = " ")
                                ) {
###
### needed for data.frame(z=vectype(...))
###
    force(nm)
    nrows <- length(x)
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) == nrows && !anyDuplicated(row.names)) {
        }
        else row.names <- .set_row_names(nrows)
    }
    if (!is.null(names(x))) 
        names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}


plot.geom <- function(x,...){
    pts = lapply(x, readWKT, p4s=crs(x))
    plot(do.call(rbind,pts))
}

geom2sp <- function(x){
    p1 = readWKT(x[1])
    if(class(p1)=="SpatialPoints"){
        return(geom2pts(x))
    }
    if(class(p1)=="SpatialPolygons"){
        return(geom2polys(x))
    }
    stop("Dont know how to convert ",class(p1))
}


geom2pts <- function(x){
    geom2any(x, SpatialPointsDataFrame, "SpatialPoints")
}

geom2polys <- function(x){
    geom2any(x, SpatialPolygonsDataFrame,"SpatialPolygons")
}
    
geom2any <- function(x, f1, c2){
    crs = crs(x)
    out = lapply(1:length(x),function(i){f1(readWKT(x[i],id=i,p4s=crs),data.frame(x=1),match=FALSE)})
    out = do.call(rbind,out)
    out = as(out,c2)
    proj4string(out)=crs
    out
}


geomTransform <- function(g,t_crs){
    sp = geom2sp(g)
    sp = spTransform(sp, t_crs)
    g = sp2geom(sp)
    g
}
