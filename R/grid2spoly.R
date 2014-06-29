grid2spoly <- function(xgrid,ygrid,proj4string=CRS(as.character(NA))){
     m <- length(xgrid)
     n <- length(ygrid)
     spts <-
SpatialPixels(SpatialPoints(cbind(rep(xgrid,n),rep(ygrid,each=m))),proj4string=proj4string)
     return(as(spts,"SpatialPolygons"))
}

poly2grid <- function(spp,nx,ny){
  b = bbox(spp)
  xgrid=seq(b[1,1],b[1,2],length=nx)
  ygrid=seq(b[2,1],b[2,2],length=ny)
  grid2spoly(xgrid,ygrid) 
}
