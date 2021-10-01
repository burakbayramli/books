********************************************************************************

  H G R I D

  (c) 1995-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the generation of structured 2-D H-grid inside a cascade. More
details are provided in Subsection 11.1.1 and in Section 12.3 of the book.


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

Makefile     - compilation options
README.txt   - what you read now
hgrid.exe    - executable for Windows 64-bit platform
hgrid.f      - source code
input        - example input file; blade coordinates represent the VKI-1 cascade
               (see below). Since flow past the blunt trailing edge of the original
               blade cannot be treated using the Euler equations, a wedge (the so-
               called cusp) was added to the trailing edge. The cusp simulates the
               separated flow region behind the trailing edge.
plot.pdf     - PDF file showing an example grid generated with "input"
plot.v2d     - plot file generated with "input"
vki1.grd     - example grid generated using "input"
vki1.top     - topology file (description of boundary conditions)
vki1_xy      - x- ,y-coordinates of VKI-1 transonic turbine blade (Kiock, R.;
               Lehthaus, F.; Baines, N.C.; Sieverding, C.H.: "The Transonic Flow
               Through a Plane Turbine Cascade as Measured in Four European Wind
               Tunnels". ASME J. Engineering Gas Turbines and Power, 108 (1986),
               pp. 277-284).
vki1_xy_cusp - x- and y-coordinates of the turbine blade with a cusp added
               and slightly smoothed

Additionally, the files:

bezier.f
length.f
sstretch.f
stretch.f
tfint.f

from the subdirectory "srccom" are needed for compilation (see below).


--------------------------------------------------------------------------------
  How to compile "hgrid"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, LD, FC, FFLAGS, and LDFLAGS) in "Makefile"
as appropriate for your computer, compiler and/or operating system. There are
examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77 compilers.
Type "make" at the prompt to compile the program. Please note that the library
"libgrid.a" (subdirectory "srccom") is required in order to link the executable.
Therefore, the sources in "srccom" have to be compiled first. All object files
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "hgrid"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. An example is provided in the file
"input". In order to run "hgrid", you have to type at the prompt:

% hgrid < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% hgrid < input

The program generates three files - grid, topology and plot data. In the
file "input" they are named as "vki1.grd", "vki1.top" and "plot.v2d". The
grid and the topology file can be read in by the structured 2-D Euler /
Navier-Stokes solver (directory "structured_2d"). Both files can also be
converted to an unstructured triangular grid by the program in
"grid_unstr_2d", to be used by the unstructured 2-D Euler / Navier-Stokes
solver (directory "unstructured_2d" or "C++/unstructured_2d"). The plot file
contains x- and y-coordinates of the grid nodes (column-wise). In the second
region, it also contains the coordinates of the original blade. The plot
file is stored in Vis2D format and can be visualized using the plotting
software available at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The meaning of the input parameters is described in the example file "input".
It is important to note that the coordinates of the blade have to be ordered
in clockwise manner beginning at the trailing edge. The trailing edge point has
to be specified twice. The given blade is rotated by the given stagger angle.
All angles are defined with respect to the x-axis and are thought positive in
the counter-clockwise direction.


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

im, jm = maximum dimensions in i-(x-) and j-(y-) direction
ip, jp = no. of grid points in i- and j-direction
ncoo   = no. of given blade coordinates
nle    = index of leading edge in i-direction
nte    = index of trailing edge in i-direction
nxa    = no. of grid cells around the blade
nxi    = no. of cells in inlet region
nxo    = no. of cells in outlet region
ny     = no. of cells in j-direction
bc()   = spline coefficients
x()    = x-coordinates of the grid
y()    = y-coordinates
xyb()  = given blade coordinates (x, y)


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

There are no common blocks or include files.
