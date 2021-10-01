********************************************************************************

 L A V A L

 (c) 1996-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the generation of quasi 1-D structured grids. Area distribution
over the x-axis corresponds to that of the Laval nozzle (see Eq. (12.2)).


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

LICENSE.txt - terms and conditions
Makefile    - compilation options
README.txt  - what you read now
laval.exe   - executable for Windows 64-bit platform
laval.f     - source code
input       - example input file
laval.grd   - grid file (generated using "input")


--------------------------------------------------------------------------------
  How to compile "laval"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, RM, LD, FC, FFLAGS, and LDFLAGS) in
"Makefile" as appropriate for your computer, compiler and/or operating system.
There are examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77
compilers. Type "make" at the prompt to compile the program. The object file
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "laval"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. An example is provided in the file
"input". In order to run "laval", you have to type at the prompt:

% laval < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% laval < input

The program generates a grid file (in the file "input" named as "laval.grd").
The grid file contains x-coordinates in the first column and cross sections
of the nozzle in the second column. The number of grid points (including the
dummy points - imax in Fig. 12.1) is stored at the beginning of the grid file.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The program requires the specification of the following parameters (see file
"input"):

- name of the grid file
- inlet area
- outlet area
- no of grid cells

Please note that the throat area A* is always 1.0, and that the length of the
nozzle is also 1.0. The throat is located at xthroat=ncthroat*dx.


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

idims    = max. possible number of grid points (including dummy points)
ib2      = last physical grid point (i=2 - the first one)
imax     = no. of physical + dummy points
ncells   = total no. of physical cells
ncthroat = no. of grid cells before the throat
a()      = cross section
a1       = inlet area
a2       = outlet area
x()      = x-coordinate
dx       = grid size in x-direction

All floating point values are stored as REAL*8.


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

The program consists only of the main routine. There are no include files or
common blocks.
