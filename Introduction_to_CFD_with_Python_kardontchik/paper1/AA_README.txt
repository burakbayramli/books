AA_README

paper1: A Python way to an undergraduate CFD course

Simulates the classical five 1D tests in Professor's Toro book:
    E. F. Toro
   "Riemann Solvers and Numerical Methods for Fluid Dynamics"
    Springer, 3rd edition, 2009
    

_____________________________________________________________________________________
Place all the files in one folder, named, say, "paper1".

To run any program open IPython, change the directory to paper1 and type, for example:

	run htest1

where htest1 is the name of the program.

The following is a description of the programs in this folder:


1) EXACT_RS: this is both a module and a program that you can run by typing

	run EXACT_RS

   It will solve exactly the 5 test cases using the EXACT RS (Riemann Solver)


2)	gtest1
	gtest2
	gtest3
	gtest4
	gtest5

	Solve the 5 test cases using a combination of  ANRS and Godunov flux.
	It will also solve using the EXACT_RS, for comparison

3)	htest1
	htest2
	htest3
	htest4
	htest5

	Solve the 5 test cases using the HLLC solver
	It will also solve using the EXACT_RS, for comparison

4)	rk_gtest1
	rk_gtest2
	rk_gtest3
	rk_gtest4
	rk_gtest5

	Solve the 5 test cases twice, once using ANRS+GODUNOV+Runge_Kutta and 
	the second time using ANRS+GODUNOV+ENO+Runge_Kutta
	It will also solve using the EXACT_RS, for comparison

5)	rk_htest1
	rk_htest2
	rk_htest3
	rk_htest4
	rk_htest5

	Solve the 5 test cases twice, once using  HLLC+Runge_Kutta and 
	the second time using HLLC+ENO+Runge_Kutta
	It will also solve using the EXACT_RS, for comparison	

The programs containing the solvers are:

	EXACT_RS
	ENO_1D
	ANRS_GODUNOV_FLUX
	ENO_GODUNOV_SOLVER
	HLLC_FLUX
	ENO_HLLC_SOLVER

The visualization tools (plottting) are in:

	VISUAL_1D

The program

	IC_PHYS

translates the specification of the initial state into all the cells
of the physical domain
	