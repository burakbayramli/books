AA_README

paper2: Performance of simple spatial-discretization methods in two dimensions

Simulates the six 2D tests in Liska-Wendroff's paper: 
    R. Liska and B. Wendroff
   "Comparison of Several Difference Schemes on 1D and 2D Test
    Problems for the Euler Euqations"
    SIAM Journal on Scientific Computing, 25, 2003, pp 995-1017


Solves the Liska-Wendroff cases using HLLC+ENO on a Cartesian domain
(it also solves the cases using only HLLC, if you wish, by setting the
 value of a parameter. See below)

___________________________________________________________________________________

To run any program open IPython, change the directory to 
	paper2_LISKA_WENDROFF_cases_HLLC_ENO
and type, for example:

	run case3

where case3 is the name of the program.

The following is a description of the programs in this folder:

1)	case3
	case4
	case6
	case12
	case15
	case17

	Solve the 6 cases in Table 4.3 in Liska-Wendroff's paper. 

All the programs have been set to solve using an initial low resolution of:
	dx = 0.04
	dy = 0.04
so that when you run them the first time you can see quickly how the program works
and the simulation results. For the simulations reported in the paper I used:
	dx = 0.01
	dy = 0.01

A useful feature of the solver is that it plots the intermediate state of the 
simulation after a given number of  iterations. This is useful to know what is
going on without waiting till the end of the simulation. The parameter that
controls the frequency of intermediate snapshots of the status is called Iplot.

When running with higher accuracy (smaller dx, dy) it is recommended to ask the
solver to plot a smaller number of snapshots of the simulation status, otherwise
the screen will be soon filled up with numerous plots. For this you set the flag
"Iplot" when calling the solver. For low resolution (dx=dy=0.04 a value of Iplot=40
will plot the intermediate results every 40 time iterations. For dx=dy=0.01 it is
recommended to set Iplot = 150 or 200. Or delete the argument completely, in which
case it will not plot any intermediate results (internally, in the solver program,
the default is set to Iplot = 9999).
 

An additional parameter it could worth playing with it for educational reasons
is 'interp':

a) interp = 0 runs the program with HLLC (without ENO), so you get an idea of the 
	      limitations of a 1st order solution

   interp = 1 runs the program with HLLC + ENO 

The solver is NOT restricted to the [0,1]x[0,1] domain. When solving other problems
you can set the dimensions of the rectangular domain at will using the variables:

	x1, x2, y1, y2

with x1 < x2 and y1 < y2

The meaning of the other programs in the folder (HLLC_FLUX, etc.) is similar to the
one given in another folder (for paper1), so it is not repeated here.

Note: the above programs are for the standard Liska-Wendroff cases. If you want to
      change the initial conditions by any rotation angle use:

	run case3_oblique

(Using case3_oblique you can easily generate all the other cases too). The 'case3_oblique'
calls the module "IC_PHYS_OBLIQUE" (that most probably you will find easier to understand
than "IC_PHYS")

Note on plots: once we enter into 2D problems you will notice that at the end of every
   simulation a lot of similar plots are printed (3 for pressure, 3 for density). The reason
is that one of the areas where one tends to spend too much time is how to plot the final
results. By plotting, for example, the pressure in different ways it helps the students
(when they look at the code that generates the plots) to learn fast different ways
one can plot the results and then choose the way or plot they prefer.

