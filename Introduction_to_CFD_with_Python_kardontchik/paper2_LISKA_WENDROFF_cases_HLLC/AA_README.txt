AA_README

AA_README

paper2: Performance of simple spatial-discretization methods in two dimensions

Simulates the six 2D tests in Liska-Wendroff's paper: 
    R. Liska and B. Wendroff
   "Comparison of Several Difference Schemes on 1D and 2D Test
    Problems for the Euler Euqations"
    SIAM Journal on Scientific Computing, 25, 2003, pp 995-1017

Solves the Liska-Wendroff cases using HLLC only (no ENO) on an ideal mesh consisting
of rectangular triangles


______________________________________________________________

NOTE: IT MAY TAKE SOME TIME TO GENERATE THE MESH (and then to process the
      mesh to generate the geometrical data needed for the solver),
      especially to generate a fine mesh (for h0 = 0.01 it may take 
      about 15 minutes). During this time it looks that nothing
      moves, so BE PATIENT !!!)

      TO AVOID AN INITIAL FRUSTRATION AND TO GET AN IMMEDIATE FAMILIARIZATION
      OF WHAT THE SIMULATOR DOES THE SIX CASES HAVE BEEN INITIALLY
      SET TO CREATE A VERY COARSE MESH, SETTING THE PARAMETER:

          h0 = 0.1

     (h0 is the triangle's cateto length)
     
      (this generates a mesh of only 200 triangles in [0,1]x[0,1] and a
       total of 1,250 triangles in the default larger computational domain)

       Remember, for a fine mesh (to get the paper's Figures) I used h0 = 0.01.
       This generates 20,000 triangles in [0,1]x[0,1] and 125,000 triangles
       in total for the complete computational domain.
_______________________________________________________________

To run any program open IPython, change the directory to 
	paper2_LISKA_WENDROFF_cases_HLLC
and type, for example:

	run case3


The following is a description of the programs in this folder:

1)	case3
	case4
	case6
	case12
	case15
	case17

	Solves the 6 cases in Table 4.3 in Liska-Wendroff's paper 

For the simulations reported in the paper I used:
	h0 = 0.01

A useful feature of the solver is that it plots the intermediate state of the 
simulation after a given number of  iterations. This is useful to know what is
going on without waiting till the end of the simulation. The parameter that
controls the frequency of intermediate snapshots of the status is called Iplot.

When running with higher accuracy (smaller h0) it is recommended to ask the
solver to plot a smaller number of snapshots of the simulation status, otherwise
the screen will be soon filled up with numerous plots. For this you set the flag
"Iplot" when calling the solver. For low resolution (h0=0.03 a value of Iplot=30
will plot the intermediate results every 30 time iterations. For h0=0.01 it is
recommended to set around Iplot = 200. Or delete the argument completely, in which
case it will not plot any intermediate results (internally, in the solver program,
the default is set to Iplot = 9999).
 
Additional explanations regarding additional features of the above programs are
immersed as comments in the above programs. 

The meaning of the other programs in the folder (HLLC_FLUX, etc.) is similar to the
one given in folder paper1, so it is not repeated here.

Note: when you see the word "GENERAL" as a suffix to a program, it means that 
      the program is used to solve in general physical domains (not necessarily 
rectangular). Similar programs but without the "GENERAL" suffix are used for 
rectangular domains only using an ideal structured mesh of triangles. For example:
	HLLC_SOLVER_TRI           
	HLLC_SOLVER_TRI_GENERAL

