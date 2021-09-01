# Factorial based on recursivity

#============================================================================
def FactRec(n):
#----------------------------------------------------------------------------
#  Returns the factorial of n using recursivity
#----------------------------------------------------------------------------
   return (n * FactRec(n-1) if (n > 1) else 1e0)

# main

n = int(input("n = "))

print("{0:d}! = {1:f}".format(n,FactRec(n)))
