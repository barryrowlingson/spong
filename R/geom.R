
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
  print(paste0(substr(x,1,10),"..."))
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
