********************************************************************************

  C H A N N E L

  (c) 1990-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the generation of a structured 2-D grid in a channel with circular
bump. Length of the channel is 2.0, its height (1/2) can be specified. Length
of the bump is 1.0, its relative height can be set. The upper boundary repre-
sents a symmetry line.


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
channel.exe - executable for Windows 64-bit platform
channel.f   - source code
channel.grd - example grid generated using "input"
channel.top - topology file (description of boundary conditions)
input       - example input file
plot.pdf    - PDF file with example grid generated with "input"
plot.v2d    - plot file generated with "input"

Additionally, the file "length.f" from the subdirectory "srccom" is needed for
compilation (see below).


--------------------------------------------------------------------------------
  How to compile "channel"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, LD, FC, FFLAGS, and LDFLAGS) in "Makefile"
as appropriate for your computer, compiler and/or operating system. There are
examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77 compilers.
Type "make" at the prompt to compile the program. Please note that the library
"libgrid.a" (subdirectory "srccom") is required in order to link the executable.
Therefore, the sources in "srccom" have to be compiled first. All object files
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "channel"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. An example is provided in the file
"input". In order to run "channel", you have to type at the prompt:

% channel < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% channel < input

The program generates three files - grid, topology and plot data. In the
file "input" they are named as "channel.grd", "channel.top" and "plot.v2d".
The grid and the topology file can be read in by the structured 2-D Euler /
Navier-Stokes solver (directory "structured_2d"). Both files can also be
converted to an unstructured triangular grid by the program in
"grid_unstr_2d", to be used by the unstructured 2-D Euler / Navier-Stokes
solver (directory "unstructured_2d" or "C++/unstructured_2d"). The plot file
contains x- and y-coordinates of the grid nodes (column-wise). It is stored
in Vis2D format and can be visualized using the plotting software available
at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The meaning of the input parameters is described in the example file "input".
It is important to note that the height of the channel is prescribed in % of
the length of the bump.


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

nx, ny   = number of grid cells in x- and y-direction (i-, j-coordinate)
beta1    = stretching ratio in x-direction (at leading and trailing edge)
beta2    = stretching ratio in y-direction
height   = 1/2 height of the channel
thick    = max. height of the circular bump
x(), y() = x- and y-coordinates of the grid points


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

There are no common blocks or include files.
