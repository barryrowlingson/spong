## 
##'
##' map(foo) -> returns class("map")
##' 
##' map(foo)+map(bar) ->  map(map(foo),bar)
##' 
##'
##' .. content for \details{} ..
##' @title 
##' @param obj 
##' @param ... 
##' @return 
##' @author Barry Rowlingson
map <- function(obj,...){

}


nholes <- function(sfp){
    length(sfp)-1
}

polyline.sfpolygon <- function(sfp){
    xy=sfp[[1]]
    t=rep(1,nrow(xy))
    last = nrow(xy)
    for(i in seq_len(nholes(sfp))){
        xy=rbind(xy, sfp[[i+1]])
        t=c(t,rep(i+1, nrow(sfp[[i+1]])),NA)
        xy=rbind(xy, sfp[[1]][last,])
    }
    cbind(xy,t)
}

polyline.sfmultipolygon <- function(sfmp){
    parts = lapply(sfmp, polyline.sfpolygon)
    nads = lapply(parts, function(l){rbind(l,c(NA,NA,NA))})
    do.call(rbind,nads)
}

