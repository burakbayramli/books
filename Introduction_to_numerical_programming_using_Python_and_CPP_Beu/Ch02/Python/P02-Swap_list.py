# Returning swapped list elements from a function

def Swap(xlist):                                   # list passed by reference
   temp = xlist[0]; xlist[0] = xlist[1]; xlist[1] = temp

# main

alist = [1e0, 2e0]                             # list of values to be swapped
Swap(alist)
print(alist)
