##' convert spolydf to spong
##'
##' .. content for \details{} ..
##' @title convert spolydf to spong
##' @param spdf a spatial poly df
##' @return a spong
##' @author Barry Rowlingson
##' @import sp
##' @import rgeos
##' @export
as.spong.SpatialPolygonsDataFrame = function(spdf){
  spdf$the_geom = writeWKT(spdf,byid=TRUE)
  class(spdf$the_geom) = c("geom")
  df = spdf@data
  attr(df,"the_geom") = "the_geom"
  class(df) = c("spong","data.frame")
  df
}


##' spong to spolydf
##'
##' .. content for \details{} ..
##' @title spong to spolydf
##' @param nsp a spong 
##' @return a spatal poly df
##' @author Barry Rowlingson
##' @export
##' @import rgeos
##' @import sp
as.SpatialPolygonsDataFrame.spong = function(nsp){
  geom_column= attr(nsp,"the_geom")
  geom = nsp[[geom_column]]
  class(nsp) = "data.frame"
  geom = lapply(geom,readWKT)
  glist = lapply(geom,function(p){p@polygons[[1]]})
  for(i in 1:length(glist)){
    glist[[i]]@ID=as.character(i)
  }
  SpatialPolygonsDataFrame(SpatialPolygons(glist),nsp,match.ID=FALSE)
}

