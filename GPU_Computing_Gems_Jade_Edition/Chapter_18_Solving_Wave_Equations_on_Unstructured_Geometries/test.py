# Pydgeon - the Python DG Environment
# (C) 2009, 2010 Tim Warburton, Xueyu Zhu, Andreas Kloeckner
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



from __future__ import division

import numpy as np

import pydgeon
from pydgeon.local import LocalDiscretization2D, JacobiGQ
from pydgeon.runge_kutta import integrate_in_time
from pydgeon.tools import make_obj_array




def main():
    from optparse import OptionParser
    parser = OptionParser(usage="Usage: %prog [options] <mesh.neu>")
    parser.add_option("--cl", action="store_true", help="use OpenCL")
    parser.add_option("-v", "--vis-every", type="int", metavar="S",
            help="visualize on-line every S steps")
    parser.add_option("--update-colors", action="store_true", 
            help="update colors in visualization (expensive)")
    parser.add_option("--profile", type="int", 
            help="set the profiling level (CL only)")
    parser.add_option("-i", "--ic", metavar="NAME",
            help="use initial condition NAME (try 'help')",
            default="gaussian")
    parser.add_option("-t", "--final-time", metavar="T",
            help="set final time", type="float",
            default=5)
    parser.add_option("-n", metavar="N", type="int", default=4,
            help="use polynomial degree N")

    options, args = parser.parse_args()
    if not args:
        parser.print_help()
        return

    ldis = LocalDiscretization2D(N=options.n)
    print "loading mesh"
    mesh = pydgeon.read_2d_gambit_mesh(args[0])

    print "building discretization"
    if options.cl:
        from pydgeon.opencl import CLDiscretization2D
        d = CLDiscretization2D(ldis, *mesh, **{"profile": options.profile})
    else:
        d = pydgeon.Discretization2D(ldis, *mesh)

    print "%d elements" % d.K

    # set initial conditions
    if options.ic == "sine":
        mmode = 3; nmode = 2
        Hx = np.zeros((d.K, d.ldis.Np))
        Hy = np.zeros((d.K, d.ldis.Np))
        Ez = np.sin(mmode*np.pi*d.x)*np.sin(nmode*np.pi*d.y)
    elif options.ic == "gaussian":
        Hx = np.zeros((d.K, d.ldis.Np))
        Hy = np.zeros((d.K, d.ldis.Np))

        min_x = np.min(d.x)
        max_x = np.max(d.x)
        min_y = np.min(d.y)
        max_y = np.max(d.y)

        x0 = min_x + 0.85*(max_x-min_x)
        y0 = min_y + 0.85*(max_y-min_y)

        x = d.x-x0
        y = d.y-y0
        r = (max_x-min_x)*0.005
        Ez = np.exp(-(x**2+y**2)/r**2)
    else:
        print "available ICs: sine, gaussian"
        return

    state = make_obj_array([Hx, Hy, Ez])
    if options.cl:
        state = make_obj_array([d.to_dev(x) for x in state])

    # compute time step size
    rLGL = JacobiGQ(0,0, d.ldis.N)[0]
    rmin = abs(rLGL[0]-rLGL[1])
    dt_scale = d.dt_scale()
    dt = dt_scale.min()*rmin*2/3

    # setup
    if options.vis_every:
        try:
            import enthought.mayavi.mlab as mayavi
        except ImportError:
            options.vis_every = 0

    if options.vis_every:
        vis_mesh = mayavi.triangular_mesh(
                d.x.ravel(), d.y.ravel(), Ez.ravel(),
                d.gen_vis_triangles())

    def vis_hook(step, t, state):
        if options.vis_every and step % options.vis_every == 0:
            if options.cl:
                Hx, Hy, Ez = [d.from_dev(x) for x in state]
            else:
                Hx, Hy, Ez = state

            vis_mesh.mlab_source.z = Ez.ravel()
            # update colors, too (expensive)
            if options.update_colors:
                vis_mesh.mlab_source.scalars = Ez.ravel()

        from time import time as wall_time
        progress_every = 20
        start_timing_at_step = progress_every
        if step % 20 == 0:
            if options.cl:
                d.queue.finish()
            if step == start_timing_at_step:
                start_time[0] = wall_time()
            elif step > start_timing_at_step:
                elapsed = wall_time()-start_time[0]
                timed_steps = step - start_timing_at_step
                time_per_step = elapsed/timed_steps

                line = ("step=%d, sim_time=%f, elapsed wall time=%.2f s,"
                        "time per step=%f s" % (
                        step, t, elapsed, time_per_step))

                if options.cl:
                    flops = 5 * (inner_rhs.flops + 4*d.K*d.ldis.Np)
                    line += " %f gflops/s" % (flops/time_per_step/1e9)

                print line


    if options.cl:
        from pydgeon.maxwell import CLMaxwellsRhs2D
        inner_rhs = CLMaxwellsRhs2D(d)

        def rhs(t, state):
            return make_obj_array(inner_rhs(*state))
    else:
        from pydgeon.maxwell import MaxwellRHS2D

        def rhs(t, state):
            return make_obj_array(MaxwellRHS2D(d, *state))

    # time loop
    print "entering time loop"
    start_time = [0]
    time, final_state = integrate_in_time(state, rhs, dt, 
            final_time=options.final_time, vis_hook=vis_hook)




if __name__ == "__main__":
    main()
