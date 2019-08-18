
def fib(N): 
    """ 
    Return a list of the first N Fibonacci numbers.
    """ 
    f0, f1 = 0, 1
    f = [1] * N
    for n in range(1, N):
        f[n] = f0 + f1
        f0, f1 = f1, f[n]

    return f

print(fib(10))
