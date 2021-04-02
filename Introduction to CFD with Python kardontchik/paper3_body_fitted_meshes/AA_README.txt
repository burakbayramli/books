AA_README

paper3: "Teaching CFD with Python"

Creates simple body-fitted meshes. For details see:
   K. A. Hoffmann and S. T. Chiang
  "Computational Fluid Dynamics", volume I, 4th edition
   published by Engineering Education System, 2000

To see some additional info open the pdf file "body_fitted_grid"



Note: in the following "run" a simulation means 
   a) open IPython
   b) go to the directory where the files are
   c) type: run filename  (for example, type: run ex1)
_______________________________________________________________________


To run the various examples open IPython, change the directory to 
paper3_body_fitted_meshes and type, for example:

	run ex1

The programs in these folder are:

	ex1 (reproduces Fig 9 in the paper)
	ex2 (Fig 10)
	ex3 (clustering in both the x- and y-directions)

The code that generates the body-fitted grid is in the file:

	STRUCTUREDMESH

Go through this small file to find a reference to the equations in Hoffmann's
book used to generated the various grids.

