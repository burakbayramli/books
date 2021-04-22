"""
2D flow in a channel around a circular cylinder
Partially taken from cylinder problem, Chapter 21, FEniCS book
uses skew-symmetric convection
writes files named restart...
"""

from dolfin import *

# Constants related to the geometry
bmarg = 1.e-3 + DOLFIN_EPS
xmin = 0.0
xmax = 2.2
ymin = 0.0
ymax = 0.41
xcenter = 0.2
ycenter = 0.2
radius = 0.05

# generate coarse mesh, refine only near cylinder
domain = Rectangle(xmin,ymin,xmax,ymax) \
         - Circle(xcenter,ycenter,radius,12)

mesh = Mesh(domain, 22)

# refine mesh twice around the cylinder
cylCenter = Point(xcenter, ycenter)
for refinements in [0,1]:
    cell_markers = CellFunction("bool", mesh)
    cell_markers.set_all(False)
    for cell in cells(mesh):
        p = cell.midpoint()
        if (xcenter/2. < p[0] < (xmax - xcenter)/2.) and \
           (ycenter/4. < p[1] < ymax - ycenter/4.) :
            cell_markers[cell] = True
    mesh = refine(mesh, cell_markers)
plot(mesh)

# timestepping
dt = .0125
endTime = 10. - 1.e-5

# kinematic viscosity
nu = .0005

# boundary conditions using a mesh function
boundaries = MeshFunction("size_t", mesh, mesh.topology().dim()-1)

# Inflow boundary
class InflowBoundary(SubDomain):
    def inside(self, x, on_boundary):
        return on_boundary and x[0] < xmin + bmarg

# No-slip boundary
class NoslipBoundary(SubDomain):
    def inside(self, x, on_boundary):
        dx = x[0] - xcenter
        dy = x[1] - ycenter
        r = sqrt(dx*dx + dy*dy)
        return on_boundary and \
               (x[1] < ymin + bmarg or x[1] > ymax - bmarg or \
                r < radius + bmarg)

# Outflow boundary
class OutflowBoundary(SubDomain):
    def inside(self, x, on_boundary):
        return on_boundary and x[0] > xmax - bmarg

# uncomment for more detail, or use PROGRESS
#set_log_level(DEBUG)

# Define function spaces
V = VectorFunctionSpace(mesh, "CG", 2)
Q = FunctionSpace(mesh, "CG", 1)
W = V * Q

# no-slip velocity b.c.
noslipBoundary = NoslipBoundary()
g0 = Constant( (0.,0.) )
bc0 = DirichletBC(W.sub(0), g0, noslipBoundary)

# inlet velocity b.c.
inflowBoundary = InflowBoundary()
g1 = Expression( ("4.*Um*(x[1]*(ymax-x[1]))/(ymax*ymax)" , "0.0"), \
                             Um=1.5, ymax=ymax)
bc1 = DirichletBC(W.sub(0), g1, inflowBoundary)

# outflow pressure b.c.
outflowBoundary = OutflowBoundary()
g2 = Constant(0.)
bc2 = DirichletBC(W.sub(1), g2, outflowBoundary)

# outflow velocity b.c., same as inlet
bc3 = DirichletBC(W.sub(0), g1, outflowBoundary)

# collect b.c.
bcs = [bc0, bc1, bc2, bc3]

# functions
(u, p) = TrialFunctions(W)
(v, q) = TestFunctions(W)
w = Function(W)
u0 = Function(V)
uplot = Function(V)  # used for plotting in loop so get only 1 frame

# weak form Stokes equation
Stokes = (inner(grad(u), grad(v)) - div(v)*p + q*div(u))*dx
f = Constant((0., 0.))
LStokes = inner(f, v)*dx

# weak form NSE
LNSE = inner(u0,v)*dx
# skew-symmetric convection term
NSE = (inner(u,v) + dt*(.5*inner(grad(u)*u0,v) - .5*inner(grad(v)*u0,u)\
                       + nu*inner(grad(u),grad(v)) - div(v)*p) + q*div(u) )*dx
# conventional convection term
#NSE = (inner(u,v) + dt*(inner(grad(u)*u0,v) \
#                       + nu*inner(grad(u),grad(v)) - div(v)*p) + q*div(u) )*dx

# initial condition comes from Stokes equation
solve(Stokes == LStokes, w, bcs,solver_parameters=dict(linear_solver="lu"))

# startup if True, restarting if False
if True:
    step = 0
    t=0
else:
    # MUST set appropriate step number
    step = -1
    # generate name and read from file
    soln_file = File("restart"+"%05d"%step+".xml")
    soln_file >> w
    # make new endTime, assuming constant dt in previous runs
    t = step*dt
    endTime += t

# Split the mixed solution using deepcopy
(u, p) = w.split(True)

plot(u,title="Initial velocity")
plot(p,"Initial pressure")

# Create files for storing solution at each step
#ufile = File("results/velocity%04d"%step+".pvd")
#pfile = File("results/pressure%04d"%step+".pvd")

#if step == 0:
#    ufile << u
#    pfile << p

# timestepping
while t < endTime * (1. + 1.e-10):
    u0.assign(u)
    t += dt
    step += 1
    solve(NSE == LNSE, w, bcs, \
       solver_parameters=dict(linear_solver="lu"))

    # split vel, pres out of w (deepcopy)
    (u, p) = w.split(True)

    # write out plotfiles
    #ufile << u
    #pfile << p

    # interactive plot
    """ 
    If plot(u), then get a new frame each time step.
    If use a single variable--as here--get only one frame for all
    time steps.
    """
    uplot.assign(u)
    plot(uplot,title="Velocity",rescale=False)

    # print a little information each step
    print "t=%f, max abs u=%e, max p=%e, v(.5,0)=%e" \
      % (t, max(abs(u.vector().array())), max(p.vector().array()), u([.35,0.2])[1] )

# write current solution to file.  No need to save pressure
soln_file = File("restart"+"%05d"%step+".xml")
soln_file << w

interactive()
