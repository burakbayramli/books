AA_README

paper3: Teaching CFD with Python

In this folder you will find examples on how to run DISTMESH to generate meshes
on simple domains

For more info on DISTMESH see the excellent tutorial in:
	P-O. Persson and G. Strang
	"A simple Mesh Generator in MATLAB"
	SIAM Rev, 46, 2004, pp 329-345
	

_______________________________________________________________________________________

To run any example open IPython, change the directory to paper3_DISTMESH where all the
files should be, and type, for example:

	run ex3b_mesh

where ex3b_mesh is the name of the program.

The programs generate examples of meshes with different shapes using DISTMESH. 
Instead of explaining what each program generates, the simplest thing is for you
to run them one after  another and see what happens.

The programs you can run are:

	ex1a_mesh
	ex1b_mesh
	ex1c_mesh
	ex2_mesh
	ex3a_mesh
	ex3b_mesh
	ex4_mesh
	ex5a_mesh
	ex5b_mesh
	ex8a_mesh
	ex8b_mesh
	ex9_ellipse_mesh
	ex10_mesh

Note: the generation of a mesh around an ellipse seems to run 'slow' because
      DISTMESH tries to make a perfect fit to the ellipse curve and for this
      it has to use Numpy's routine 'fmin' (a minimizer), that is very costly

Suggested procedure for a new problem (different from the test cases above):

When you begin developing a new mesh, I would suggest you initially set Iflag=3 when
calling distmesh:

p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,Iflag=3)

This will generate a plot of the mesh every time a new Delaunay triangulation
is computed (plotting slows down the simulation a lot, so use Iflag=3 only
sparingly, essentially only when you begin a new mesh problem). The plotting
of the consecutive Delaunay triangulations will give you soon an intuitive picture
whether you are going in the correct path towards getting a usable mesh. If you see
that DISTMESH seems to be "stalling" with no appreciable progress, it is time to
abort the program and try something else. This could mean, for example, adding some
fixed points to the boundary to help DISTMESH understand what your intent is, or
changing the size function fh, ... or something else: it is not guaranteed that
DISTMESH will always give you a good mesh or that it will converge to such mesh.

Once you see from the plots that DISTMESH is beginning to generate reasonable meshes
it is time to switch to Iplot=4. This will skip the (slow) plot generation, but will
continue printing for each new Delaunay triangulation the minimum q and the minimum
triangle angle. This will give you very soon an idea of the achievable q-values.
DISTMESH will try to achieve the "optimum" and this could take a long time. So, once
you see the values of minimum q that are obtained in successive Delaunay triangulations,
it is simpler to abort the simulation and set a minimum q acceptable to you and
tell DISTMESH so by including in the call to DISTMESH what this acceptable qmin
is. For example:

qmin = 0.7
p,t,bars = distmesh(fd,fh,h0,xmin,ymin,xmax,ymax,pfix,Iflag=4,qmin=qmin)

With qmin set (its default value was 1.0), DISTMESH will stop once this qmin is achieved
and will output the generated triangulation data (p,t,bars) needed by the Euler solver.


