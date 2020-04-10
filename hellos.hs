
 ind::Char -> String ->Int
 ind  x [] = (-1)
 ind  x (h:t)
      | x==h =0
      | m < 0 = (-1)
      | otherwise = 1 + m
    where m = index x t      



 index:: Char -> String -> Int
 index x y =  if y == []
                 then (-1)
     else if x == head y then 0
     else if index x (tail y ) <0 then (-1)
     else 1+ index x (tail y)

 hexToDec::String->Int
 hexToDec x = if  x == [] then 0 
  else (index (head x) "0123456789abcdef") * 16 ^ (length x - 1) + hexToDec (tail x)
 