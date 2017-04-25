from ode_FE import ode_FE

def test_ode_FE():
    """Test that a linear u(t)=a*t+b is exactly reproduced."""

    def exact_solution(t):
        return a*t + b

    def f(u, t):  # ODE
        return a + (u - exact_solution(t))**m

    a = 4
    b = -1
    m = 6

    dt = 0.5
    T = 20.0

    u, t = ode_FE(f, exact_solution(0), dt, T)
    diff = abs(exact_solution(t) - u).max()
    tol = 1E-15           # Tolerance for float comparison
    success = diff < tol
    assert success

test_ode_FE()
