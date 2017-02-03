def Newton(f, x, dfdx, epsilon=1.0E-7, N=100, store=False):
    f_value = f(x)
    n = 0
    if store: info = [(x, f_value)]
    while abs(f_value) > epsilon and n <= N:
        dfdx_value = float(dfdx(x))
        if abs(dfdx_value) < 1E-14:
            raise ValueError("Newton: f'(%g)=%g" % (x, dfdx_value))

        x = x - f_value/dfdx_value

        n += 1
        f_value = f(x)
        if store: info.append((x, f_value))
    if store:
        return x, info
    else:
        return x, n, f_value


def _g(x):
    return exp(-0.1*x**2)*sin(pi/2*x)

def _dg(x):
    return -2*0.1*x*exp(-0.1*x**2)*sin(pi/2*x) + \
           pi/2*exp(-0.1*x**2)*cos(pi/2*x)

def _test():
    from scitools.std import sin, cos, exp, linspace, plot, pi
    import sys

    x0 = float(sys.argv[1])
    x, info = Newton(_g, x0, _dg, store=True)
    print 'root: %.16g' % x
    for i in range(len(info)):
        print 'Iteration %2d: f(%g)=%g' % \
              (i, info[i][0], info[i][1])

    x = linspace(-7, 7, 401)
    y = _g(x)
    plot(x, y)

if __name__ == '__main__':
    _test()


