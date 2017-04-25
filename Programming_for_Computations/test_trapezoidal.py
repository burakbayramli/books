from trapezoidal import trapezoidal

def test_trapezoidal_one_exact_result():
    """Compare one hand-computed result."""
    from math import exp
    v = lambda t: 3*(t**2)*exp(t**3)
    n = 2
    numerical = trapezoidal(v, 0, 1, n)
    exact = 2.463642041244344
    err = abs(exact - numerical)
    tol = 1E-14
    success = err < tol
    msg = 'error=%g > tol=%g' % (err, tol)
    assert success, msg

def test_trapezoidal_linear():
    """Check that linear functions are integrated exactly."""
    f = lambda x: 6*x - 4
    F = lambda x: 3*x**2 - 4*x  # Anti-derivative
    a = 1.2; b = 4.4
    exact = F(b) - F(a)
    tol = 1E-14
    for n in 2, 20, 21:
        numerical = trapezoidal(f, a, b, n)
        err = abs(exact - numerical)
        success = err < tol
        msg = 'n=%d, err=%g' % (n, err)
        assert success, msg

def convergence_rates(f, F, a, b, num_experiments=14):
    from math import log
    from numpy import zeros
    exact = F(b) - F(a)
    n = zeros(num_experiments, dtype=int)
    E = zeros(num_experiments)
    r = zeros(num_experiments-1)
    for i in range(num_experiments):
        n[i] = 2**(i+1)
        numerical = trapezoidal(f, a, b, n[i])
        E[i] = abs(exact - numerical)
        if i > 0:
            r_im1 = log(E[i]/E[i-1])/log(float(n[i])/n[i-1])
            r[i-1] = float('%.2f' % r_im1)  # Truncate to two decimals
    return r

def test_trapezoidal_conv_rate():
    """Check empirical convergence rates against the expected -2."""
    from math import exp
    v = lambda t: 3*(t**2)*exp(t**3)
    V = lambda t: exp(t**3)
    a = 1.1; b = 1.9
    r = convergence_rates(v, V, a, b, 14)
    print r
    tol = 0.01
    assert (abs(r[-1]) - 2) < tol, r[-4:]
