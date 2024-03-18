***************************************************************************

To compile:

gfortran -o p3.out program3.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPS.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT


rm CPS.DAT 
./p3.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPS.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







