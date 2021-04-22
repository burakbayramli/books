from fe_approx1D import approximate

from sympy import Symbol, sin, tanh, pi
x = Symbol('x')

def approx(functionclass='intro'):
    """
    Exemplify approximating various functions by various choices
    of finite element functions
    """
    import os
    if functionclass == 'intro':
        # Note: no plot if symbol=True
        approximate(x*(1-x), symbolic=False,
                    d=1, N_e=2, filename='fe_p1_x2_2e')
        #approximate(x*(1-x), symbolic=True, numint='Trapezoidal',
        #            d=1, N_e=2, filename='fe_p1_x2_2eT')
        approximate(x*(1-x), symbolic=False,
                    d=1, N_e=4, filename='fe_p1_x2_4e')
        approximate(x*(1-x), symbolic=False,
                    d=2, N_e=1, filename='fe_p2_x2_1e')
        #approximate(x*(1-x), symbolic=True, numint='Simpson',
        #            d=2, N_e=4, filename='fe_p1_x2_2eS')
        approximate(x*(1-x), symbolic=False,
                    d=1, N_e=4, filename='fe_p1_x2_4e')

        # Only the extended fe_approx1D_numint can do P0 elements
        import fe_approx1D_numint
        fe_approx1D_numint.approximate(x*(1-x), symbolic=False,
                    d=0, N_e=4, filename='fe_p0_x2_4e')
        fe_approx1D_numint.approximate(x*(1-x), symbolic=False,
                    d=0, N_e=8, filename='fe_p0_x2_8e')
        for ext in 'pdf', 'png':
            cmd = 'doconce combine_images fe_p1_x2_2e.%(ext)s fe_p1_x2_4e.%(ext)s fe_p1_x2_2e_4e.%(ext)s' % vars()
            os.system(cmd)
            cmd = 'doconce combine_images fe_p0_x2_4e.%(ext)s fe_p0_x2_8e.%(ext)s fe_p1_x2_4e_8e.%(ext)s' % vars()
            os.system(cmd)

    elif functionclass == 'special':
        # Does not work well because Heaviside cannot be analytically
        # integrated (not important) and numpy cannot evaluate
        # Heaviside (serious for plotting)
        from sympy import Heaviside, Rational
        approximate(Heaviside(x - Rational(1,2)), symbolic=False,
                    d=1, N_e=2, filename='fe_p1_H_2e')
        approximate(Heaviside(x - Rational(1,2)), symbolic=False,
                    d=1, N_e=4, filename='fe_p1_H_4e')
        approximate(Heaviside(x - Rational(1,2)), symbolic=False,
                    d=1, N_e=8, filename='fe_p1_H_8e')
    elif functionclass == 'easy':
        approximate(1-x,        symbolic=False,
                    d=1, N_e=4, filename='fe_p1_x_4e')
        approximate(x*(1-x),    symbolic=False,
                    d=2, N_e=4, filename='fe_p2_x2_4e')
        approximate(x*(1-x)**8, symbolic=False,
                    d=1, N_e=4, filename='fe_p1_x9_4e')
        approximate(x*(1-x)**8, symbolic=False,
                    d=1, N_e=8, filename='fe_p1_x9_8e')
        approximate(x*(1-x)**8, symbolic=False,
                    d=2, N_e=2, filename='fe_p2_x9_2e')
        approximate(x*(1-x)**8, symbolic=False,
                    d=2, N_e=4, filename='fe_p2_x9_4e')
        for ext in 'pdf', 'png':
            cmd = 'doconce combine_images fe_p1_x9_4e.%(ext)s fe_p1_x9_8e.%(ext)s fe_p1_x9_4e_8e.%(ext)s'
            os.system(cmd)
            cmd = 'doconce combine_images fe_p2_x9_2e.%(ext)s fe_p2_x9_4e.%(ext)s fe_p2_x9_2e_4e.%(ext)s'
            os.system(cmd)
            cmd = 'doconce combine_images fe_p1_x9_4e.%(ext)s fe_p2_x9_2e.%(ext)s fe_p1_x9_8e.%(ext)s fe_p2_x9_4e.%(ext)s fe_p1_p2_x9_248e.%(ext)s'
            os.system(cmd)

    elif functionclass == 'hard':
        approximate(sin(pi*x),  symbolic=False,
                    d=1, N_e=4, filename='fe_p1_sin_4e')
        approximate(sin(pi*x),  symbolic=False,
                    d=1, N_e=8, filename='fe_p1_sin_8e')
        approximate(sin(pi*x),  symbolic=False,
                    d=2, N_e=2, filename='fe_p2_sin_2e')
        approximate(sin(pi*x),  symbolic=False,
                    d=2, N_e=4, filename='fe_p2_sin_4e')
        approximate(sin(pi*x)**2,     symbolic=False,
                    d=1, N_e=4, filename='fe_p1_sin2_4e')
        approximate(sin(pi*x)**2,     symbolic=False,
                    d=1, N_e=4, filename='fe_p1_sin2_8e')
        approximate(sin(pi*x)**2,     symbolic=False,
                    d=2, N_e=2, filename='fe_p2_sin2_2e')
        approximate(sin(pi*x)**2,     symbolic=False,
                    d=2, N_e=4, filename='fe_p2_sin2_4e')
        for ext in 'pdf', 'png':
            cmd = 'doconce combine_images fe_p1_sin_4e.%(ext)s fe_p1_sin_8e.%(ext)s fe_p1_sin_4e_8e.%(ext)s'
            os.system(cmd)
            cmd = 'doconce combine_images fe_p2_sin_2e.%(ext)s fe_p2_sin_4e.%(ext)s fe_p2_sin_2e_4e.%(ext)s'
            os.system(cmd)
            cmd = 'doconce combine_images fe_p1_sin2_4e.%(ext)s fe_p1_sin2_8e.%(ext)s fe_p1_sin2_4e_8e.%(ext)s'
            os.system(cmd)
            cmd = 'doconce combine_images fe_p2_sin2_2e.%(ext)s fe_p2_sin2_4e.%(ext)s fe_p2_sin2_2e_4e.%(ext)s'
            os.system(cmd)
import sys
try:
    task = sys.argv[1]
except IndexError:
    print('Usage: %s approx_intro | approx_easy | approx_hard' % sys.argv[0])
    sys.exit(1)

approx(task.split('_')[1])


