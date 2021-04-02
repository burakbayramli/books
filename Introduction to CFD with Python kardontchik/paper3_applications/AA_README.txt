AA_README

paper3: Teaching CFD with Python


Note: in the following "run" a simulation means 
   a) open IPython
   b) go to the directory where the files are
   c) type: run filename  (for example, type: run tri_mesh
___________________________________________________________________

"paper3_applications" is a messy directory of files: in all the other
directories the geometry of the computational domain is very simple,
so when running one file, the file creates the mesh and then simulates
the problem. 

We still do this when you run Toro's explosion test problem:

	run toro_explosion

or when you run Toro's wedge problem:

	run toro_wedge
	(or run rk_toro_wedge, if you use Runge-Kutta)

However, then come other more complicated practical applications. These
include here:

    b) A blunt-nose
    c) An airfoil

#######################################
Note: 
In practical applications usually one generates the mesh separately and
stores the generated mesh in a file. Then when one runs the Euler simulator
you first load the file with the mesh data and then simulate. Mesh generation
usually is done in a separate directory than the directory where one runs
the Euler simulator. However, since I do not know how you will finally order
your folders in your computer, I preferred to put everything (mesh generation
and Euler simulation) in one single directory to be sure that when you run
the simulation Python will not stop it saying that "some files are missing".
Putting mesh generation and Euler simulation files in one single directory
makes this directory messy, but safer for the first-time user.
########################################

For the blunt nose the geometry is still simple, so you run:
	run blunt_mesh_unif
to create the mesh and store the data mesh in a file, and then you run
	run blunt_unit_sim
to load that file with the mesh data and run the Euler simulator

The situation becomes more complicated for an airfoil, as explained in the paper.
And this makes this directory look messy. To help a bit initially I have already
run the files needed to create the mesh for an airfoil at an angle of attack of
1.25 degrees, so if you wish to run the Euler simulator on the airfoil you can
just type directly:

	run airfoil_M0x8_1x25deg_sim  (for 1st order forward difference)
	run rk_airfoil_M0x8_1x25deg_sim   (for Runge-Kutta)

However, I also detail a step-by-step procedure to generate the mesh first.

------------------------------------------

For first time use, the simple way is to generate the figures in the paper
one after another to see what is done and how, step by step.This is explained below:


Figure 1: run toro_explosion
	(run initially with a coarse grid: h0 = 0.08 to get a feeling)
	(to reproduce the figure in the paper run with h0 = 0.02)
	(adjust accordingly the parameter Iplot to avoid cluttering the screen)

Figure 2: is obtained together with Figure 4 and 5
Figure 3: NA
Figures 4 and 5: run toro_wedge (uses 1st order forward difference)
	         run rk_toro_wedge (uses 3rd order Runge-Kutta)
	(run initially with a coarse grid, setting h0 = 1)
	(to reproduce the figure in the paper use h0 = 0.25)

Figure 6 (left): run blunt_comp_domain
Figure 6 (right): run blunt_mesh_unif
	Note: blunt_mesh_unif runs 'slow' because we want to use an
	      exact fit to the cubic cylinder curve. For this we have
	      to use Numpy's routine 'fmin', a minimizer)

	      
Figures 7 and 8: run blunt_unif_sim (uses 1st order forward difference)
	         run rk_blunt_unif_sim (uses 3rd order Runge-Kutta)

Figure 9 and 10: go to directory "paper3_body_fitted_meshes" and read AA_README

For meshing in Figures 11-15 run the following in the stated order:

Figure 11 and 12: run tri_mesh
Figure 13: run quad_mesh
Figure 14: run total_mesh
Figure 15: run flat_mesh

Figures 16 and 17: run airfoil_M0x8_1x25deg_sim  (for 1st order forward difference)
		   run rk_airfoil_M0x8_1x25deg_sim   (for Runge-Kutta)


(these two files load the needed mesh for a 1.25 degrees angle of attack)

Or you can begin from zero and create first the mesh running
successively the following:

a) run mesh1_1x25deg  (# iter = 41, CPU = 220 sec)
b) run qmesh1_1x25deg
c) run flat_mesh1_1x25 

and then, after the flat mesh has been created and saved you can run:

d) airfoil_M0x8_1x25deg_sim  (for 1st order forward difference)

or

e) rk_airfoil_M0x8_1x25deg_sim   (for Runge-Kutta)

For Figure 18 (and much more): go to directory "paper3_DISTMESH" and read there AA_README
_________________________
For more info about airfoils see also the file: README_airfoil
