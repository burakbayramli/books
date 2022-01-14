

NUMERICA,
A Library of Source Codes for 
Teaching, Research and Applications
by E. F. Toro
NUMERITEK Limited, 1999, UK

HYPER-WAT Sub-library, Version 1.0, 2000.


INTRODUCTION

Welcome to the HYPER-WAT sub-library. All files in these directories 
are DOS formatted, i.e. they conform to the extended DOS character set 
and can thus be used directly on any IBM compatible PC equipped with a 
FORTRAN compiler. Conversion to UNIX can be done, for example, by

  to_unix file1 file2
  
where file1 is the DOS file and file2 is the UNIX file.

This sub-library contains thirteen FORTRAN 77 source codes. Five of these 
solve the time-dependent one-dimensional linearised shallow water equations.
The other eight programs solve the non-linear shallow water equations in one 
and two space dimensions. The programming language is STANDARD FORTRAN 77, the 
only exception being the statement IMPLICIT NONE. These source codes originate 
from research and teaching codes developed by the author over a period of many 
years of involvement in research, applications and teaching activities. The 
codes have been used as demonstration codes in regular courses on numerical 
methods given to MSc students in two British Universities and in short courses 
on Computational Fluid Dynamics given in several countries.


THE PROGRAMS

The 13 programs of HYPER-WAT are found in the 3 directories 

linwat.dir: one-dimensional linearised shallow water equations
onedim.dir: one-dimensional shallow water equations
twodim.dir: two-dimensional shallow water equations


Each FORTRAN 77 program "progra.f" is self-contained and only requires 
an input data file "progra.ini". Numerical results are printed to an 
output file "progra.out". The programs can be copied directly to any 
computer with a FORTRAN 77 compiler and used immediately. The input 
file "progra.ini" can be edited and changed as desired. The input 
files for the one-dimensional programs in "onedim.dir" directory contain 
a selection of several test problems. The user can simply CUT the 
desired test and PASTE it at the beginning of the file before executing 
the program. All the test problems have been carefully selected and offer 
various degrees of difficulty to numerical methods. Some of the tests 
are exceedingly challenging and we warn the user that these  will cause 
some of the programs to "crash". This will depend on the particular 
scheme used and on the computational parameters, such as Courant number. 


VISUALISATION

There are a selection of commercial plotting packages that can be used 
to visualise the numerical results. For simplicity, we assume the user 
has access to the public domain plotting package "GNUPLOT". This can 
be downloaded from the web, visit http://www.gnu.org/ for more information. 

The one-dimensional codes produce output of the solution at a specified 
time TIMEOU. The output is printed in the form of columns:

x1      y1    z1   ........

where x1 is a position in the spatial domain, y is the corresponding 
value of a quantity y (eg density) and z1 is that of a quantity z 
(eg pressure).  The profile (x, y) can be plotted by first typing 
<gnuplot>. Then, within the GNUPLOT environment type:

<plot 'progra.out' u 1:2> This will plot the profile using symbols.

Similarly,  to plot the profile (x, z) one types:

<plot 'progra.out' u 1:3>

To produce both symbols and a line one types:

<plot 'progra.out' u 1:2,'progra.out' u 1:2 w l>

For the two-dimensional codes one can also plot the solution at a 
given output time and for a constant x or y value. These results 
can also be plotted using the GNUPLOT scripts in "gnu2d.txt" in 
the form of surface plots or contours. A simialr script is available 
for the three-dimensional codes, "gnu3d.txt" in the threedim directory.  
The user is encouraged to exploit the capabilities of GNUPLOT by 
reading the appropriate GNUPLOT documentation. Alternatively, she/he 
can use more sophisticated visualisation software available in their 
systems.

In the OUTPUT routine for the two-dimensional codes we also print
out the results as required by the commercial graphics package TECPLOT, 
in case the user has access to it.


GRID GENERATION

The two-dimensional codes are set up to solve the equations 
in very simple domains and using very simple meshes. Realistic 
applications will require the use of more sophisticated meshes, for 
which the user is expected to write her/his own mesh generator or use 
commercially available mesh generation packages.


SUPPORT

Users on support agreements can contact NUMERITEK LIMITED regarding 
any matter connected to the NUMERICA programs by emailing:

support@numeritek.com
  
All users are encouraged to report bugs or any problems they may encounter 
by emailing:  

bugs@numeritek.com


IMPORTANT NOTICE:

NUMERICA is the copyright of NUMERITEK LIMITED and its use is governed 
by the terms of the License Agreement accompanying this package.  
 
If you cannot locate the License Agreement please write to NUMERITEK 
LIMITED at:

PO Box 68
Bramhall 
Cheshire 
SK7 3FT
United Kingdom 

or email inquiries@numeritek.com for a copy.  


© NUMERITEK LTD., 2000
