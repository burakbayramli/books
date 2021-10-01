********************************************************************************

  U G R I D

  (c) 2000-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the conversion of 2-D structured grids (in STRUCT2D format) into
unstructured triangular grids in UNSTR2D format. The resulting unstructured
grid can be read in by the unstructured flow solver contained in the
directories "unstructured_2d" or "CPP/unstructured_2d" (see Sections 12.7 or
12.9).


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
channel.grd - structured grid for channel with circular bump
channel.top - topology file for channel grid
channel.ugr - unstructured triangular grid generated from "channel.*"
input_c     - example input file for channel with circular bump
input_n     - example input file for NACA 0012 airfoil
input_v     - example input file for VKI-1 turbine blade
n0012.grd   - structured grid for NACA 0012 airfoil
n0012.top   - topology file for NACA 0012 grid
n0012.ugr   - unstructured triangular grid generated from "n0012.*"
plot.pdf    - pdf file showing various triangulated grids
plot_c.v2d  - plot file generated with "input_c"
plot_n.v2d  - plot file generated with "input_n"
plot_v.v2d  - plot file generated with "input_v"
ugrid.exe   - executable for Windows 64-bit platform
ugrid.f     - source code
vki1.grd    - structured grid for VKI-1 turbine blade
vki1.top    - topology file for VKI-1 grid
vki1.ugr    - unstructured triangular grid generated from "vki1.*"


--------------------------------------------------------------------------------
  How to compile "ugrid"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, LD, FC, FFLAGS, and LDFLAGS) in "Makefile"
as appropriate for your computer, compiler and/or operating system. There are
examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77 compilers.
Type "make" at the prompt to compile the program. All object files and the
executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "ugrid"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. An example is provided in the file
"input". In order to run "ugrid", you have to type at the prompt:

% ugrid < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% ugrid < input

The program generates a file with the triangulated grid (in the file
"input_c" named as "channel.ugr"), and a plot file (named as "plot_c.v2d").
The plot file contains x- and y-coordinates of the grid nodes (column-wise).
It is stored in Vis2D format and can be visualized using the plotting software
available at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The meaning of the input parameters is described in the example file
"input". It is important to make sure that the structured grid and the
topology file both closely follow the STRUCT2D format. Otherwise, "ugrid"
may not function properly.


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

im, jm   = max. dimensions of the structured grid in i-, j-direction
mxsegs   = max. no. of boundary segments (structured grid)
mxbfaces = max. no. of boundary faces (unstructured grid)
mxbnodes = max. no. of boundary nodes (unstructured grid)
il, jl   = dimensions of the structured grid in i-, j-direction
nsegs    = no. of boundary segments (structured grid)
nnodes   = no. of grid nodes (unstructured grid)
ncells   = no. of triangles
nbounds  = no. of boundary conditions
bface()  = 2 nodes which define a boundary face (unstructured grid);
           bnode() used at periodic boundaries instead
bname()  = designation of the boundary condition (unstructured grid)
bnode()  = 2 nodes which are periodic
btype()  = code of boundary condition:
           100-199 = inflow
           200-299 = outflow
           300-399 = no-slip wall (identical to slip wall here)
           400-499 = slip wall (inviscid flow)
           500-599 = symmetry line (501 - x=xonst., 502 - y=const.)
           600-699 = far-field
           700-799 = interior cut / periodic boundary
           800-899 = injection boundary
ibound() = faces and nodes which belong to a particular boundary:
           (_,1) - last index in bface()
           (_,2) - last index in bnode()
lbsegs() = description of boundary conditions (7 quantities per segment):
           (_,1) = code of the boundary condition (see btype() above)
           (_,2) = side of the computational domain (1-4; see Fig. 13.2)
           (_,3) = begin of the segment (cell index in computational space)
           (_,4) = end of the segment
           (_,5) = side of the computational domain to which the "source"
                   segment belongs, i.e., another cut or periodic boundary which
                   communicates with the current segment
           (_,6) = begin of the source segment
           (_,7) = end of the source segment
pnt()    = pointer from node (i,j) to the corresponding node of the unstructured
           grid
tria()   = 3 nodes which define a triangular cell
x(), y() = x-, y-coordinates of the structured grid
xyu()    = x-, y-coordinates of the unstructured grid


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

The program does not contain any common blocks or include files.
