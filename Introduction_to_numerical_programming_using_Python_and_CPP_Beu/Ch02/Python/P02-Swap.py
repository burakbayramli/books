# Returning swapped arguments from a function

def Swap(x, y):                            # scalar arguments passed by value
   temp = x; x = y; y = temp
   return (x, y)                                 # return interchanged values

# main

a = 1e0; b = 2e0                                       # values to be swapped
(a,b) = Swap(a,b)      # left side - return values; right side - input values
print(a,b)
