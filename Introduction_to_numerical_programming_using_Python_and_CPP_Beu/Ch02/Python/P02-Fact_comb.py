# Calculates combinations using a factorial function

#============================================================================
def Fact(n):
#----------------------------------------------------------------------------
#  Returns the factorial of n
#----------------------------------------------------------------------------
   f = 1e0
   for i in range(2,n+1): f *= i
   return f

# main

n = int(input("n = "))
k = int(input("k = "))

C = Fact(n)/(Fact(k)*Fact(n-k))

print("C({0:d},{1:d}) = {2:f}".format(n,k,C))
