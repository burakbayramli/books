# README

Dr. Kardontchik kindly shared these codes with me whose content
closely follow Toro's book Riemann Solvers and Numerical Methods for
Fluid Dynamics.

Reference papers:

1) “A Python way to an undergraduate CFD course”

2) “Performance of simple spatial-discretization methods in two dimensions”

3) “Teaching CFD with Python”

The code to run all the examples in the papers is sent in different
zipped files that should be placed in separated folders. These folders
are:

a) Paper1

b) Paper2_LISKA_WENDROFF_cases_HLLC_ENO

c) Paper2_LISKA_WENDROFF_cases_HLLC

d) Paper3_DISTMESH

e) Paper3_body_fitted_meshes

f) Paper3_applications


In every folder you will find a text file named AA_README with a
description of what the files in that folder do and how to run them.

For first-time users I recommend the following sequence, based on the
criterion that is best to begin with short simulations (to understand
the mechanics) and end with long simulations:

1) Begin with the one-dimensional simulations in folder Paper1 (Toro’s
5 classical tests).  These simulations run very fast, usually less
than 1 minute each

2) Then go to the folder Paper3_DISTMESH and run the simulations to
obtain the meshes of simple domains (1-2 minutes each). This will let
you become familiar with Persson’s DISTMESH mesh generator.

3) Then go to any of the two folders that run the simulations of
Liska-Wendroff’s six 2D cases. Although these simulations might last
up to 30 minutes (when run with a very fine mesh), they are simple
since the mesh is simple and you can run them first in a few minutes
by using a coarse mesh first. These simulations introduce you to the
world of 2D simulations so you can begin getting familiar with the
graphical displays used for 2D.

4) Finally, you can go to the folder Paper3_applications to run the
more complex files dealing with Toro’s explosion and wedge tests, the
blunt-nosed cylinder and the NACA0012 airfoil.

IF YOU USE THE CODE OR MY PAPERS IN YOUR WORK WITH YOUR PEERS, OR WITH
STUDENTS OR IN YOUR PUBLICATIONS, KINDLY ACKNOWLEDGE THEIR USE BY
REFERRING TO MY PAPERS AS THEY APPEARED AT http://www.researchgate.net
NO PART OF THE CODE SHOULD BE USED FOR COMMERCIAL PURPOSES. IT IS
INTENDED TO BE USED ONLY AT EDUCATIONAL INSTITUTIONS.  Jaime
E. Kardontchik, PhD Sunnyvale, CA