#! /usr/bin/env python
# $Id: demo.py 299 2007-03-30 12:52:17Z mhagger $

# Copyright (C) 1999-2003 Michael Haggerty <mhagger@alum.mit.edu>
#
# This file is licensed under the GNU Lesser General Public License
# (LGPL).  See LICENSE.txt for details.

# demo.py -- Demonstrate the Gnuplot python module.


import Gnuplot, Gnuplot.funcutils

def demo(): 
    g = Gnuplot.Gnuplot(debug=1)
    g.title('A simple example') # (optional)
    g('set data style linespoints') # give gnuplot an arbitrary command
    # Plot a list of (x, y)  (tuples or a numpy array would also be OK):
    g.plot([[0,1.1], [1,5.8], [2,3.3], [3,4.2]])
    raw_input('Please press return to continue...\n')

    g.reset()
    x = arange(10, dtype='float_')
    y1 = x**2
    d = Gnuplot.Data(x, y1, title='Calculated by Python', with_='points 3 3')
    g.title('Data can be computed by Python or gnuplot')
    g.xlabel('x')
    g.ylabel('x squared')
    # Plot a function alongside the Data PlotItem defined above:
    g.plot(Gnuplot.Func('x**2', title='calculated by gnuplot'), d)
    raw_input('Please press return to continue...\n')

    # Save what we just plotted as a color postscript file.
    g.ylabel('x^2')  
    g.hardcopy('gp_test.ps', enhanced=1, color=1)
    print ('\n**** Saved plot to postscript file "gp_test.ps" ******\n')
    raw_input('Please press return to continue...\n')

    g.reset()
    # Demonstrate a 3-d plot: 
    x = arange(35)/2.0
    y = arange(30)/10.0 - 1.5
    # Make a 2-d array containing a function of x and y.  First create
    # xm and ym which contain the x and y values in a matrix form 
    xm = x[:,newaxis]
    ym = y[newaxis,:]
    m = (sin(xm) + 0.1*xm) - ym**2
    g('set parametric')
    g('set data style lines')
    g('set hidden')
    g('set contour base')
    g.title('An example of a surface plot')
    g.xlabel('x')
    g.ylabel('y')
   
    g.splot(Gnuplot.GridData(m,x,y, binary=0))
    raw_input('Please press return to continue...\n')

    # plot another function, but letting GridFunc tabulate its values
    def f(x,y):
        return 1.0 / (1 + 0.01 * x**2 + 0.5 * y**2)

    g.splot(Gnuplot.funcutils.compute_GridData(x,y, f, binary=0))
    raw_input('Please press return to continue...\n')
    
# when executed, just run demo():
if __name__ == '__main__':
    demo()
    
