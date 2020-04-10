isd::Int->[Int]->Bool
isd 
isd x y 
    | mod x 2 ==0 =False 
    | mod x (head y) == 0 =False
    | otherwise =isd x (tail y)