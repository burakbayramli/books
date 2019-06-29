def polynomial(xx, x, y):
    """ Polynomial interpolation of points (x[i],y[i]) to find y(xx).
    Warning: the values of y[] are modified."""

    N = len(x)
    for n in range(1, N):
        for i in range(N-n):
            y[i] = (xx-x[i])*y[i+1]+(x[i+n]-xx)*y[i]
            y[i] /= x[i+n]-x[i]
    return y[0]
