"""
2D flow in a channel around a circular cylinder
Partially taken from cylinder problem, Chapter 21, FEniCS book
Uses Chorin & Temam's solution method
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
dt = .00125
endTime = 3. - 1.e-5

# kinematic viscosity
nu = .001

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
bc0u = DirichletBC(V, g0, noslipBoundary)

# inlet velocity b.c.
inflowBoundary = InflowBoundary()
g1 = Expression( ("4.*Um*(x[1]*(ymax-x[1]))/(ymax*ymax)" , "0.0"), \
                             Um=1.5, ymax=ymax)
bc1 = DirichletBC(W.sub(0), g1, inflowBoundary)
bc1u = DirichletBC(V, g1, inflowBoundary)

# outflow pressure b.c.
outflowBoundary = OutflowBoundary()
g2 = Constant(0.)
bc2 = DirichletBC(W.sub(1), g2, outflowBoundary)
bc2p = DirichletBC(Q, g2, outflowBoundary)

# outflow velocity b.c., same as inlet
bc3 = DirichletBC(W.sub(0), g1, outflowBoundary)
bc3u = DirichletBC(V, g1, outflowBoundary)

# collect b.c.
bcs = [bc0, bc1, bc2, bc3]

# functions
(us, ps) = TrialFunctions(W)
(vs, qs) = TestFunctions(W)

# weak form Stokes equation
Stokes = (inner(grad(us), grad(vs)) - div(vs)*ps + qs*div(us))*dx
f = Constant((0., 0.))
LStokes = inner(f, vs)*dx

# initial condition comes from Stokes equation
w = Function(W)
solve(Stokes == LStokes, w, bcs,solver_parameters=dict(linear_solver="lu"))

# Split the mixed solution using deepcopy
(uinit, pinit) = w.split(True)

umax = max(abs(uinit.vector().array()))
print "Worst possible Courant number, initial velocity=",dt*umax/mesh.hmin()

# Timestepping method starts here

step = 0
t=0

u0 = Function(V)
u0.assign(uinit)
utilde = Function(V)
u1 = Function(V)
p1 = Function(Q)

# Define test and trial functions
v = TestFunction(V)
q = TestFunction(Q)
u = TrialFunction(V)
p = TrialFunction(Q)

# STEP 1 (u will be utilde)
F1 = (1./dt)*inner(v, u - u0)*dx + inner(v, grad(u0)*u0)*dx \
        + nu*inner(grad(u), grad(v))*dx - inner(f, v)*dx
a1 = lhs(F1)
L1 = rhs(F1)

# STEP 2 (p)
a2 = inner(grad(q), grad(p))*dx
L2 = -(1./dt)*q*div(utilde)*dx

# STEP 3 (Velocity update)
a3 = inner(v, u)*dx
L3 = inner(v, utilde)*dx - dt*inner(v, grad(p1))*dx

# Assemble matrices
# cannot use symmetric b.c.!
A1 = assemble(a1)
bc0u.apply(A1)
bc1u.apply(A1)
bc3u.apply(A1)
A2 = assemble(a2)
bc2p.apply(A2)
A3 = assemble(a3)
bc0u.apply(A3)
bc1u.apply(A3)
bc3u.apply(A3)

# set up 3 quiet solvers for fixed matrices
solver1 = LUSolver(A1)
solver1.parameters["reuse_factorization"] = True
solver1.parameters["report"] = False

solver2 = LUSolver(A2)
solver2.parameters["reuse_factorization"] = True
solver2.parameters["report"] = False

solver3 = LUSolver(A3)
solver3.parameters["reuse_factorization"] = True
solver3.parameters["report"] = False

# timestepping
while t < endTime * (1. + 1.e-10):
    t += dt
    step += 1

    # STEP 1 solution: solve for utilde
    b = assemble(L1)
    bc0u.apply(b)
    bc1u.apply(b)
    bc3u.apply(b)
    solver1.solve( utilde.vector(), b )

    # STEP 2 solution: solve for p1
    b = assemble(L2)
    bc2p.apply(b)
    solver2.solve( p1.vector(), b )

    # STEP 3 solution: solve for u1
    b = assemble(L3)
    bc0u.apply(b)
    bc1u.apply(b)
    bc3u.apply(b)
    solver3.solve( u1.vector(), b )

    # prepare for next time step
    u0.assign(u1)

    plot(u0,title="Velocity",rescale=False)

    # print a little information each step
    print "t=%f, max abs u=%e, max p=%e, v(.5,0)=%e" \
      % (t, max(abs(u1.vector().array())), \
         max(p1.vector().array()), u1([.35,0.2])[1] )
umax = max(abs(u1.vector().array()))
print "Worst possible Courant number=",dt*umax/mesh.hmin()

interactive()
