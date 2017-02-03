from diff2Dmesh import diff2Dmesh
from scitools.std import *

def input_handling():
    """Return data read from the command line."""
    import getopt
    try:
        options, args = getopt.getopt(
            sys.argv[1:], '',
            ['f=', 'xmin=', 'xmax=', 'ymin=', 'ymax=',
             'nx=', 'ny=', 'exact-dfdx=', 'exact-dfdy='])
    except getopt.GetoptError, e:
        print 'Error in command-line option:\n', e
        sys.exit(1)

    # Default values
    f = None
    xmin = ymin = 0.0
    xmax = ymax = 1.0
    nx = ny = 10
    exact_dfdx = exact_dfdy = None

    for option, value in options:
        if option == '--f':
            f = StringFunction(value,
                               independent_variables=('x', 'y'))
        elif option == '--xmin':
            xmin = eval(value)
        elif option == '--ymin':
            ymin = eval(value)
        elif option == '--xmax':
            xmax = eval(value)
        elif option == '--ymax':
            ymax = eval(value)
        elif option == '--nx':
            nx = int(value)
        elif option == '--ny':
            ny = int(value)
        elif option == '--exact-dfdx':
            exact_dfdx = StringFunction(value,
                         independent_variables=('x', 'y'))
        elif option == '--exact-dfdy':
            exact_dfdy = StringFunction(value,
                         independent_variables=('x', 'y'))
    if f is None:
        raise ValueError('No function was specified!')
    return f, xmin, xmax, ymin, ymax, nx, ny, \
           exact_dfdx, exact_dfdy
            
            
def main():
    f, xmin, xmax, ymin, ymax, nx, ny, \
       exact_dfdx, exact_dfdy = input_handling()

    x, y, dfdx, dfdy = \
       diff2Dmesh(f, xmin, xmax, ymin, ymax, nx, ny)

    # plot:
    pt = mesh  # plot type
    xv, yv = ndgrid(x, y)
    pt(xv, yv, dfdx, title='df/dx', 
       view=[40,40], colorbar='on',
       hardcopy='tmp_dfdx.eps')
    figure()
    pt(xv, yv, dfdy, title='df/dy', 
       view=[40,40], colorbar='on',
       hardcopy='tmp_dfdy.eps')

    if exact_dfdx is not None:
        figure()
        # Use same mesh as for the approximations,
        # use vectorized computations for exact_dfdx
        exact_dfdx.vectorize(globals())
        pt(xv, yv, exact_dfdx(xv, yv), 
           view=[40,40], title='Exact df/dy',
           colorbar='on', hardcopy='tmp_dfdx_exact.eps')
    if exact_dfdy is not None:
        figure()
        exact_dfdy.vectorize(globals())
        pt(xv, yv, exact_dfdy(xv, yv), 
           view=[40,40], title='Exact df/dy',
           colorbar='on', hardcopy='tmp_dfdy_exact.eps')

# python diff2Dmesh_inputviz.py --f "x**2+y**2" --nx 2 --ny 2
main()
