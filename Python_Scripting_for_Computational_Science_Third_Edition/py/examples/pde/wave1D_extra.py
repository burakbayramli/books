#!/usr/bin/env python
"""
Classes for solving a 1D wave equation.
Alternatives (less attractive) to SolverWithViz in wave1D_class.
"""
from __future__ import division  # disable integer division
from scitools.numpyutils import *

from wave1D_class import WaveEq1, WaveEq2

# use of Graphics1D instead of CurveViz directly:

import Graphics1D

class SolverWithViz_:
    def __init__(self, solver):
        self.s = solver
        self.g = Graphics1D.Graphics1D()

    def do_graphics(self):
        if self.g.graphics:
            if self.g.plot_prm['xcoor'] is not self.s.x or \
               self.g.graphics.coor is not self.s.x:
                print 'Warning: xcoor in graphics object not bound to x array'
                self.g.grid(self.s.x)

            self.g.graphics.plotcurve(self.s.up,
                 legend='u(x,t=%g)' % self.s.t,
                 ps=self.g.plot_prm['hardcopy']=='ps')
            
    def action(self, solver):
        print 't=',solver.t
        self.do_graphics()
        self.solutions.append(self.s.up.copy())


def test_plug_WaveEq1_(plot=1, version='scalar', n=50):
    """
    Use class SolverWithViz, which has a solver and
    a Graphics1D instance as attributes.
    """
    L = 1
    c = 1
    tstop = 2
    def I(x):
        """Plug profile as initial condition."""
        if abs(x-L/2.0) > 0.1:
            return 0
        else:
            return 1

    w = SolverWithViz_(WaveEq1())
    w.s.usage()
    if plot:
        w.g.set(graphics=True)
    else:
        w.g.set(graphics=None)
    if plot == 2:
        w.g.set(hardcopy='ps')
    w.s.set(I=I, f=0, bc_0=0, bc_L=0, c=1, n=n, tstop=2,
            user_action=w.action, scheme_coding=version)
    w.g.set(ymin=-1.1, ymax=1.1)  # fix y axis
    # when n is set, bind x to graphics:
    w.g.grid(w.s.x)
    w.s.set(f=lambda x,t: 0) # speeds up the code significantly (twice as fast)
    # showed up in profile - vis profile!
    w.s.dump()
    w.solutions = []
    w.s.solve_problem()
    print 'CPU time:', w.s.cpu
    if not allclose(w.solutions[0], w.solutions[-1],
                    atol=1.0E-10, rtol=1.0E-12):
        print 'error in computations'
        print w.solutions[0]-w.solutions[-1]
        if plot:
            g.plotcurves([(w.solutions[0],'t=0'),
            (w.solutions[-1],'t=%g' % w.s.numerical_prm['tstop'])])
    else:
        print 'correct solution'


# multiple inheritance - this approach is not recommended
class TestPlugWaveEq1(WaveEq1, Graphics1D.Graphics1D):
    """Not successful; set in base and this class interfere."""
    def __init__(self):
        WaveEq1.__init__(self)
        Graphics1D.__init__(self)
        self._prm_list.append(self.plot_prm)

    def action(self, self_):
        self.solutions.append(self.up.copy())
        if self.do_plot(self.t):
            self.graphics.plotcurve(self.up,
               legend='u(x,t=%g)' % self.t, ps=\
               self.plot_prm['hardcopy']=='ps')

    def I(self, x):
        """Plug profile as initial condition."""
        if abs(x-L/2.0) > 0.1:
            return 0
        else:
            return 1

    def main(self, plot=0, version='scalar', n=50):
        self.solutions = []
        L = 1
        c = 1
        tstop = 2
        self.usage()
        if plot:
            self.set(graphics=True)
        else:
            self.set(graphics=None)
        if plot == 2:
            self.set(hardcopy='ps')
        self.set(I=self.I, f=0, bc_0=0, bc_L=0, c=1, n=n, tstop=2,
                 ymin=-1.1, ymax=1.1,  # fix y axis
                 user_action=self.action, scheme_coding=version)
        self.set(f=lambda x,t: 0) # speeds up the code significantly (twice as fast)
        # showed up in profile - vis profile!
        self.dump()
        solutions = []
        self.solve_problem()
        print 'CPU time:', self.cpu
        if not allclose(solutions[0], solutions[-1],
                        atol=1.0E-10, rtol=1.0E-12):
            print 'error in computations'
            print solutions[0]-solutions[-1]
            if plot:
                self.graphics.plotcurves([(solutions[0],'t=0'),
                (solutions[-1],'t=%g' % w.numerical_prm['tstop'])])
        else:
            print 'correct solution'

def test_plug_TestPlugWaveEq1(plot=1, version='scalar', n=50):
    """Use class with WaveEq1 and Graphics1D as base classes."""
    s = TestPlugWaveEq1()
    s.main(plot, version, n)
    

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_plug_TestPlugWaveEq1 1 "'vectorized'" """ % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
