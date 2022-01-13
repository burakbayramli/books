def fac(x):
    if (x == 0):
        return 1
    else:
        return x*fac(x-1)


a = fac(52)
print a
print a.bit_length()


