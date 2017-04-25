from nonlinear_solvers import rate

def print_rates(method, x, x_exact):
    q = ['%.2f' % q_ for q_ in rate(x, x_exact)]
    print method + ':'
    for q_ in q:
        print q_,
    print

if __name__ == '__main__':
    print_rates('Newton', x = [1, 2, 3, 4, 5], x_exact = 3)
