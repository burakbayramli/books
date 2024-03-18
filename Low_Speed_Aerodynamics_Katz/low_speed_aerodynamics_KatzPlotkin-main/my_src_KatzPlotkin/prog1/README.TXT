***************************************************************************

To compile:

gfortran -o p1.out program1.f

***************************************************************************



***************************************************************************

To run:

Before running erase the files AFOIL2.DAT and CP.DAT

Before running:
	* You need to erase the files AFOIL2.DAT and CP.DAT

Input values:
	* Input values for airfoil (VAN DE VOOREN TRANSFORMATION), e.g.,

	Airfoil like shape:
		* Thickness 0.15
		* E.E. angle 1.8 - Bounded between 1 and 2
		* AOA 0
		* Panels 120

	Elliptical shape:
		* Thickness 0.15
		* E.E. angle 1.2 - Bounded between 1 and 2
		* AOA 0
		* Panels 120



rm CP.DAT 
rm AFOIL2.DAT
./p1.out

***************************************************************************



***************************************************************************

To plot results in gnuplot:

set datafile separator ','
plot plot.csv using 1:2 with lines, '' using 1:3 with lines

set datafile separator ','
plot "CP.DAT" using 1:2 with lines
plot "AFOIL2.DAT" using 1:2 with lines

***************************************************************************







