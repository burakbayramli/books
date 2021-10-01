********************************************************************************

  C G R I D

  (c) 1995-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the generation of a structured 2-D C-grid about an airfoil. More
details are provided in Subsections 11.1.1, 11.1.3 and in Section 12.3 of
the book.


This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


--------------------------------------------------------------------------------
  Files
--------------------------------------------------------------------------------

Makefile    - compilation options
README.txt  - what you read now
cgrid.exe   - executable for Windows 64-bit platform
cgrid.f     - source code
input       - example input file for inviscid grid around NACA 0012 airfoil
input_v     - example input file for viscous grid around NACA 0012 airfoil
input_rg15a - example input file for inviscid grid around RG 15A-1.8/11.0 airfoil
n0012.grd   - example grid generated using "input"
n0012.top   - topology file (description of the boundary conditions)
n0012_v.grd - example grid generated using "input_v"
n0012_v.top - topology file (description of the boundary conditions)
rg15a.grd   - example grid generated using "input_rg15a"
rg15a.top   - topology file (description of the boundary conditions)
output      - screen dump (inviscid grid)
output_v    - screen dump (viscous grid)
output_rg   - screen dump (inviscid grid)
plot.pdf    - PDF file with example grid generated with "input"
plot.v2d    - plot file generated with "input"
plot_v.pdf  - PDF file with example grid generated with "input_v"
plot_v.v2d  - plot file generated with "input_v"
plot_rg.v2d - plot file generated with "input_rg15a"
plot_rg.pdf - PDF file with example grid generated with "input_rg15a"

Additionally, the files:

bezier.f
ellgrid.f
length.f
sstretch.f
stretch.f
tfint.f

from the subdirectory "srccom" are needed for compilation (see below). The
files "output*" were generated using the corresponding "input*" files.


--------------------------------------------------------------------------------
  How to compile "cgrid"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, LD, FC, FFLAGS, and LDFLAGS) in "Makefile"
as appropriate for your computer, compiler and/or operating system. There are
examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77 compilers.
Type "make" at the prompt to compile the program. Please note that the library
"libgrid.a" (subdirectory "srccom") is required in order to link the executable.
Therefore, the sources in "srccom" have to be compiled first. All object files
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "cgrid"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. An example is provided in the file
"input". In order to run "cgrid", you have to type at the prompt:

% cgrid < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% cgrid < input

The program generates three files - grid, topology and plot data. In the
file "input" they are named as "n0012.grd", "n0012.top" and "plot.v2d". The
grid and the topology file can be read in by the structured 2-D Euler /
Navier-Stokes solver (directory "structured_2d"). Both files can also be
converted to an unstructured triangular grid by the program in
"grid_unstr_2d", to be used by the unstructured 2-D Euler / Navier-Stokes
solver (directory "unstructured_2d" or "C++/unstructured_2d"). The plot file
contains x- and y-coordinates of the grid nodes (column-wise). In the second
region, it also stores the original coordinates of the airfoil. Plot files
are stored in Vis2D format and can be visualized using the plotting software
available at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The meaning of the input parameters is described in the example file "input".
It is important to note that the coordinates of the airfoil have to be ordered
in clockwise manner beginning at the trailing edge. The trailing edge point has
to be specified twice. The coordinates have to be scaled between 0 and 1. If
generating viscous grids, it is advisable to start directly with elliptic
smoothing (i.e., the parameter "Laplace smoothing" should be set to 0).


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

im, jm   = maximum dimensions in i-(x-) and j-(y-) direction
ip, jp   = no. of grid points in i- and j-direction
ncoo     = no. of given airfoil coordinates
nle      = index of leading edge in i-direction
ntel     = index of "lower" trailing edge in i-direction
nteu     = index of "upper" trailing edge in i-direction
nxa      = no. of grid cells around airfoil
nxw      = no. of cells in wake (i-direction)
ny       = no. of cells in j-direction
angles() = angles between grid lines and boundary (j=1, j=jp)
bc()     = spline coefficients
distes() = distances between first grid line and boundary (j=1, j=jp)
p(),q()  = control functions (elliptic grid generation)
x()      = x-coordinates of the grid
y()      = y-coordinates
xya()    = given airfoil coordinates (x, y)


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

There are no common blocks or include files.
