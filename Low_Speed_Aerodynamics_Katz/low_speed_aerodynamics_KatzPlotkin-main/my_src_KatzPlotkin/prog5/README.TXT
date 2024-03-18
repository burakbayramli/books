***************************************************************************

To compile:

gfortran -o p5.out program5.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPV.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT
	* AOA


rm CPV.DAT 
./p5.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPV.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







