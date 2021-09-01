def Sum(x, y):                                   # returns the sum of x and y
   s = x + y
   return s

def Average(x, n):                 # returns the average of x[1] through x[n]
   s = 0e0
   for i in range(1,n+1): s += x[i]
   s /= n
   return s
