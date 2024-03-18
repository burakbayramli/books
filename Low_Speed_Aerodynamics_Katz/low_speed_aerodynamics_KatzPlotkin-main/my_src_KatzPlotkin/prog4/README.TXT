***************************************************************************

To compile:

gfortran -o p4.out program4.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPD.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT
	* AOA


rm CPD.DAT 
./p4.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPD.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







