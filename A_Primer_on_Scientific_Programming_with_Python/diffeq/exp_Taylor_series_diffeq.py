
def exp_diffeq(x, N):
    n = 1
    an_prev = 1.0  # a_0
    en_prev = 0.0  # e_0
    while n <= N:
        en = en_prev + an_prev
        an = x/n*an_prev
        en_prev = en
        an_prev = an
        n += 1
    return en

# Direct evaluation of the sum
def exp_sum(x, N):
    from scitools.std import factorial
    e = 0
    for n in range(N):
        e += x**n/factorial(n)
    return e

def test():
    import sys, math
    x = float(sys.argv[1])
    print 'exp(%g)=%g:' % (x, math.exp(x))
    for N in 2, 4, 8, 16, 32, 64, 128:
        print 'N=%3d exp_diffeq=%10.4e  exp_sum=%10.4e' % \
              (N, exp_diffeq(x, N), exp_sum(x, N))
test()

