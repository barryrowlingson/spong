test_that("geom classes", {

    s1 = "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"
    s2 = "POLYGON ((2 2, 2 3, 3 3, 3 2, 2 2))"
    s3 = "POLYGON ((2 0, 3 1, 2 1, 2 0))"
    
    w = readWKT(s1)
    
    g1 = geom(s)
    crs(g1)="+init=epsg:4326"

    g2 = geom(s, "+init=epsg:4326")
    expect_identical(g1,g2)
        
    g1 = geom(c(s1,s2,s3))
    crs(g1)="+init=epsg:4326"
    g2 = geom(c(s1,s2,s3))
    crs(g2)="+init=epsg:4326"
    expect_identical(g1,g2)

    # check sub set keeps crs:
    gs = g1[2:3]
    expect_equal(crs(gs),crs(g1))
    
}
          )
