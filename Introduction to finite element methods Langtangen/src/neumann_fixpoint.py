import matplotlib.pyplot as plt 
from dolfin import * 

def boundary(x, on_boundary): 
  if near(x[0],0.3) and near(x[1],0): return True
  return False

mesh = UnitSquareMesh(10,10) 
V = FunctionSpace(mesh, "Lagrange", 1) 
u = TrialFunction(V) 
v = TestFunction(V)

n = FacetNormal(mesh)
f = Expression("pow(3.14,2)*cos(3.14*x[0])+1", degree=4) 
ue = Expression("cos(3.14*x[0])-1", degree=4)
du = Expression(["3.14*sin(3.14*x[0])", "0"], degree=4)

a = dot(grad(v), grad(u))*dx 
L = v*f*dx + inner(du,n)*v*ds 

point_condition = DirichletBC(V, ue, boundary, "pointwise")
u = Function(V, name="u")
solve(a == L, u, point_condition) 


# plot 
plot(u)
plt.show()

ue = interpolate(ue, V)
ue.rename("ue", "")
plot(ue)
plt.show()




