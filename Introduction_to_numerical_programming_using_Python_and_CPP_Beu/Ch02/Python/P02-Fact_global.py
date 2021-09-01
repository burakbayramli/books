# Factorial with global variables

def Factorial():
   global f, n                                            # global variables
   f = 1e0
   for i in range(2,n+1): f *= i

# main

n = int(input("n = "))

Factorial()

print(n,"! = ",f)
