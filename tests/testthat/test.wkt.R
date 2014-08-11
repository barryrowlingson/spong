samples = list(
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

for(ss in samples){
    sftd = parseWKT(ss$text)
    expect_equal(attr(sftd,"type"), ss$type)
    expect_equal(attr(sftd,"dimension"), ss$dim)
}


      
polyhole = "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))"

multipolygon = "MULTIPOLYGON(
  (
  (-0.94 0.66,
   -0.30 0.90,
    0.25 0.44,
   -0.38 -0.23,
   -0.79 0.00,
   -0.79 0.00,
   -0.94 0.66)
 ,
 (-0.68 0.42,-0.59 0.18,-0.18 0.49,-0.68 0.42)
)
,
(
(-0.18 -0.41,0.26 0.01,0.98 -0.66,-0.18 -0.41),(0.14 -0.38,0.17 -0.24,0.38 -0.28,0.38 -0.45,0.14 -0.38))

)"

linestring = "LINESTRING (30 10, 10 30, 40 40)"

multilinestring = "MULTILINESTRING ((10 10, 20 20, 10 40),
(40 40, 30 30, 40 20, 30 10))"

# MULTIPOLYGON   |--- POLYGON  ----|      |--- POLYGON ---|
# (              (( pairs ),(pairs))  ,   ((pairs),(pairs))  )

mp = parseWKT(multipolygon)
expect_error(c(mp,mp))

