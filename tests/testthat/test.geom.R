test_that("geom classes", {

    s1 = parseWKT("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))")
    s2 = parseWKT("POLYGON ((2 2, 2 3, 3 3, 3 2, 2 2))")
    s3 = parseWKT("POLYGON ((2 0, 3 1, 2 1, 2 0))")
    
    
    g1 = geom1(s1)
    crs(g1)="+init=epsg:4326"

    g2 = geom1(s1, "+init=epsg:4326")
    expect_identical(g1,g2)
        
    g1 = geom(list(s1,s2,s3))
    crs(g1)="+init=epsg:4326"
    g2 = geom(list(s1,s2,s3))
    crs(g2)="+init=epsg:4326"
    expect_identical(g1,g2)

    # check sub set keeps crs:
    gs = g1[2:3]
    expect_equal(crs(gs),crs(g1))
    
}
          )
