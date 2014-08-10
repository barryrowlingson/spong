## g = geom_from_points(SpatialPoints(
##     cbind(311421+100*runif(10),501269+100*runif(10))
##     ,proj4string=CRS("+init=epsg:27700")))

## ng = data.frame(name=letters[1:10],
##     the_geom=g
##     )

test_that("spong data frames", {

    s1 = parseWKT("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))")
    s2 = parseWKT("POLYGON ((2 2, 2 3, 3 3, 3 2, 2 2))")
    s3 = parseWKT("POLYGON ((2 0, 3 1, 2 1, 2 0))")

    g = geom(list(s1,s2,s3))

    d1 = data.frame(gg=g, m=c(33,22,99))
    d2 = data.frame(gg=g, m=c(33,22,99))
    the_geom(d1)="gg"
    d2 = setgeom(d2, "gg")
    expect_identical(d1,d2)
    
})
