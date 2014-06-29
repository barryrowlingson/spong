s1 = "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"
s2 = "POLYGON ((2 2, 2 3, 3 3, 3 2, 2 2))"
s3 = "POLYGON ((2 0, 3 1, 2 1, 2 0))"

w = readWKT(s1)

g = geom(s)
crs(g)="+init=epsg:4326"

g2 = geom(c(s1,s2,s3))
crs(g2)="+init=epsg:4326"

d = data.frame(i=1:3)
d$g = g2
the_geom(d)="g"
