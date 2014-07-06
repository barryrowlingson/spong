S = list(
    list(text = "POINT (1 1)",
         type="POINT",
         dim=2,
         measure=FALSE,
         body="(1 1)"),
    list(text = "point (1 1)",
         type="POINT",
         dim=2,
         measure=FALSE,
         body="(1 1)"),
    list(text = "POINT Z (1 2 3)",
         type="POINT",
         dim=3,
         measure=FALSE,
         body="(1 2 3)"),
    list(text = "POINT M (1 2)",
         type="POINT",
         dim=2,
         measured=TRUE,
         body="(1 2)"
         ),
    list(text = "POINT ZM ( 1 2 3 4)",
         type="POINT",
         dim=3,
         measured=TRUE,
         body="( 1 2 3 4)"
         ),
    list(text="MULTIPOLYGON (((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))",
         type="MULTIPOLYGON",
         dim=2,
         measure=FALSE,
         body="(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))"),
    list(text="MULTIPOLYGON z(((1 1 1,5 1 1,5 5 1,1 5 1,1 1 1),(2 2 2,2 3 2,3 3 2,3 2 2,2 2 2)),((6 3 2,9 2 2,9 4 2,6 3 2)))" ,
         type="MULTIPOLYGON",
         dim=3,
         body="(((1 1 1,5 1 1,5 5 1,1 5 1,1 1 1),(2 2 2,2 3 2,3 3 2,3 2 2,2 2 2)),((6 3 2,9 2 2,9 4 2,6 3 2)))",
         measure=FALSE)
    )

for(ss in S){
    sftd = parseWKT_head(ss$text)
    expect_equal(sftd$type, ss$type)
    expect_equal(sftd$dim, ss$dim)
}
    
      
polyhole = "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))"
