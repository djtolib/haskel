import R2Graph
myCir::Triangle->(R2Point,Double)
myCir t =
       let 
         a = vertex0 t
         b = vertex1 t
         c = vertex2 t
         midab = R2Point ((x a + x b)/2) ((y a + y b)/2)
         midac = R2Point ((x a + x c)/2) ((y a + y c)/2)
         n1 = normal (subtractPoints a b)
         n2 = normal (subtractPoints a c)
         (Just cen) = intersectLines midab n1 midac n2
       in (cen, distance cen a)
                  