from dolfin import * 

mesh = UnitSquareMesh(10,10) 
L = FiniteElement("Lagrange", mesh.ufl_cell(), 1) 
R = FiniteElement("R", mesh.ufl_cell(),  0)
W = FunctionSpace(mesh, L*R)

(u,c) = TrialFunction(W) 
(v,d) = TestFunction(W)

n = FacetNormal(mesh)

f = Expression("pow(3.14,2)*cos(3.14*x[0])+1", degree=4) 
ue = Expression("cos(3.14*x[0])-1", degree=4)
du = Expression(["3.14*sin(3.14*x[0])", "0"], degree=4)

a = dot(grad(v), grad(u))*dx + c*v*dx  + d*u*dx  
L = v*f*dx + inner(du,n)*v*ds 

w = Function(W)
solve(a == L, w) 
(u,c) = w.split()

u.rename("u lagrange", "")
ufile = File("u_lagrange.pvd")
ufile << u


