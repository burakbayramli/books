***************************************************************************

To compile:

gfortran -o p6.out program6.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPLS.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT
	* Enter coordinate of wake point. Usually the TE but not yet.  Choose a 
	  value a lit bit smaller. For example, if the TE is at 1, choose 0.999


rm CPLS.DAT 
./p6.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPLS.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







