***************************************************************************

To compile:

gfortran -o p11.out program11.f

***************************************************************************



***************************************************************************

To run:

Before running:
	* You need the file AFOIL2.DAT with the airfoil coordinates
	* Erase the file CPQD.DAT

Input values:
	* Number of panels - Must be equal to the number of points
	  in the file AFOIL2.DAT
	* AOA
	* 'ENTER X COORD. OF INTERNAL POINT'. Type a value close to the LE coordinates.
	  The value must be lower than the coordinates of the LE. In this case 0.999 is a good choice.	


rm CPQD.DAT 
./p11.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot "CPQD.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







