   
   fib::(Int,[Integer]) -> (Int,[Integer])
   fib (0,y)     = (0,y)
   fib (n,[])    = fib (n-1,[1])
   fib (n,[1])   = fib (n-1,[1,1])
   fib (n,a:b:t) = fib (n-1,a+b:a:b:t)
   
   fibonacci::Int->[Integer]
   fibonacci n =  reverse (snd (fib (n,[])))