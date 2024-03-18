***************************************************************************

To compile:

gfortran -o p9.out program9.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPSD.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT
	* AOA


rm CPSD.DAT 
./p9.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPSD.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







