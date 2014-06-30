g = geom_from_points(SpatialPoints(
    cbind(311421+100*runif(10),501269+100*runif(10))
    ,proj4string=CRS("+init=epsg:27700")))

ng = data.frame(name=letters[1:10],
    the_geom=g
    )
