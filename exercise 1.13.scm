Method 1:

Fib(n) 
   = 0 for n=0
   = 1 for n=1
   = 1 for n=2

g(n) = phi^n/sqrt(5) where phi = (1 + sqrt(5))/2
   = 0.44 for n=0
   = 0.723 for n=1
   = 1.17 for n=2
  
Thus, the proposition holds for i=0,1,2

   Assume that Fib(k) = round (g(k)) and Fib(k-1) = round(g(k-1)) for any k
   Then,
   Fib(k+1) = Fib(k) + Fib(k-1) = round(g(k)) + round(g(k-1)) 
   g(k+1) = phi*g(k), 
   g(k) = g(k+1)/phi
   Also,
   g(k) = phi*g(k-1)
   g(k-1) = g(k)/phi = g(k+1)/phi^2
Thus,
   Fib(k+1) = round(g(k+1)/phi) + round(g(k+1)/phi^2)
   Since phi is an irrational number, any number multiplied by phi can never have a fractional part of exactly 0.5
   Hence we can rule out the occurence of a possibility such as: round(x.5) + round(y.5) <> round(x.5 + y.5), or x + y <> x+y+1
   Thus, we can safely write
   Fib(k+1) = round( (1/phi + 1/phi^2) * g(k+1)) = round(1.00 * g(k+1)) = round (g(k+1))

Method 2:
                                                              
   Assume psi = (1-sqrt(5))/2 = -0.618
   
   Assume that Fib(k) = (phi^k - psi^k)/sqrt(5) & 
   Fib(k-1) = (phi^k-1 - psi^k-1)/sqrt(5) for any k
   Then,
   Fib(k+1) = Fib(k) + Fib(k-1)
   = (phi^k - psi^k)/sqrt(5) + (phi^k-1 - psi^k-1)/sqrt(5)
   = (phi^k + phi^k-1 - (psi^k + psi^k-1))/sqrt(5)
   = (phi^k+1 (1/phi + 1/phi^2) - psi^k+1(1/psi + 1/psi^2))/sqrt(5)
   = (phi^k+1 - psi^k+1)/sqrt(5)
   
   abs(psi^n) is a number always less than 1.  Hence Fib(n) is the closest integer to 
   (phi^n)/sqrt(5)