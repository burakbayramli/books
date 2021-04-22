"""
example9.py
from dolfin-demos/documented/csg-2D/python
CGAL-type mesh generation
"""

from dolfin import *

# Define 2D geometry
domain = Rectangle(0., 0., 5., 5.) - \
    Rectangle(2., 1.25, 3., 1.75) - Circle(1, 4, .25) - Circle(4, 4, .25)
domain.set_subdomain(1, Rectangle(1., 1., 4., 3.))
domain.set_subdomain(2, Rectangle(2., 2., 3., 4.))

# Generate and plot mesh
mesh2d = Mesh(domain, 45)
plot(mesh2d, "2D mesh")

# Convert subdomains to mesh function for plotting
mf = MeshFunction("size_t", mesh2d, 2, mesh2d.domains())
plot(mf, "Subdomains")

interactive()
