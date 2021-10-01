********************************************************************************

  U N S T R U C T 2 D

  (c) 1997-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the solution of 2-D Euler and Navier-Stokes equations on
unstructured triangular grids. Median-dual finite-volume scheme is employed
for the spatial discretisation together with Roe's upwind scheme. The
equations are integrated in time using an explicit multistage time-stepping
scheme. The program also offers the possibility to simulate incompressible
flows at very low Mach numbers. Further details are provided in Subsection
12.7 of the book.


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

doc - HTML documentation (files, modules, functions and variables; point your
      browser to doc/html/index.html)
run - test cases (grids and topology files, user input files, results)
src - source code

The following test cases are provided in the directory "run":

channel  - inviscid, transonic flow in a channel with 10% circular bump
fplate   - laminar viscous flow past a flat plate
n0012    - inviscid, transonic flow around NACA 0012 airfoil
n4415_20 - inviscid, incompressible flow around NACA 4415 airfoil with flap
vki1     - inviscid, transonic flow inside VKI-1 turbine cascade (Kiock, R.;
           Lehthaus, F.; Baines, N.C.; Sieverding, C.H.: "The Transonic Flow
           Through a Plane Turbine Cascade as Measured in Four European Wind
           Tunnels". ASME J. Engineering Gas Turbines and Power, 108 (1986),
           pp. 277-284)

Files with the extension "ugr" contain the grid. Files with "_input" added
to the case name represent user input files. Furthermore, "iso" in the name
signifies a plot file containing the complete 2-D flow-field, whereas "surf"
stands for the surface only. The directory "run" contains in "Unstruct2D.exe"
the executable program for 64-bit Windows platforms (Win 7 and higher).


--------------------------------------------------------------------------------
  How to compile "Unstruct2D"
--------------------------------------------------------------------------------

Compilation of the sources in "src" requires a modern C++ compiler (C++11).
The best option on Windows is to employ Microsoft's Visual Studio, for which
the solution file is included. The freely available Express Editions of
Visual Studio (12 and higher) are sufficient. As an alternative, or on other
platforms, GNU's freely available g++ compiler is also recommendable. On
Windows, the compiler is provided in the MinGW package. An easy to install
distribution can be downloaded at: www.tdm-gcc.tdragon.net/download. It
offers the possibility to compile the sources to either 32- or 64-bit
executable.


--------------------------------------------------------------------------------
  How to run "Unstruct2D"
--------------------------------------------------------------------------------

The program requires a number of user input parameters which determine the
fluid properties, set the boundary conditions, control the program
execution, and the output of the results. The user parameters are stored in
a plain ASCII file. Examples are provided in the files "*_input_*" in the
directory "run". In order to run "Unstruct2D", open a terminal window
(Command Prompt on Windows), change to the directory where the grid and user
input files are located and type at the prompt:

> Unstruct2D input_file

where "input_file" is the path and name of the file with user parameters
(e.g., "channel_input"). Depending on how your environment is set up (PATH
variable), you might have to specify the full path to the executable as
well. During and after the run, the program generates four output files:
convergence history, data for isoplots, distribution of flow quantities
along wall(s) and a restart file. The data in the convergence and in the
both plot files are stored in Vis2D format and can be visualized using the
commercial plotting software available from CFD Consulting & Analysis
(www.cfd-ca.de).

In the case of external flows, file with the convergence history contains the
following quantities:

step   - iteration number
resid  - density based convergence measure (see Eq. (12.1))
resmax - maximum difference between new and old density [kg/m^3]
i-res  - index of the grid node where resmax is found
cl     - lift coefficient   }
cd     - drag coefficient   } considering pressure forces only!
cm     - moment coefficient }

Lift, drag and moment coefficients are replaced by:

mass-flow  - average mass flow rate [kg/s]
mass-ratio - ratio of mass flow at outlet to mass flow at inlet

for internal flows.

The file for the generation of isoplots always contains the grid coordinates
(denoted as x, y). Additionally, the following quantities are written out for
each grid point, if switched on in the user input file (last section):

density  - density [kg/m^3]
u        - x-component of the velocity [m/s]
v        - y-component of the velocity [m/s]
p        - static pressure [Pa]
p-tot    - total pressure [Pa]
T        - static temperature [K]
T-tot    - total temperature [K]
M        - local Mach number
M-isen   - isentropic Mach number
pt-loss  - total pressure loss
visc-lam - laminar viscosity coefficient (dynamic) [kg/ms]
cf       - skin friction coefficient (boundaries only)
-Cp      - pressure coefficient (boundaries only)

Variables in the convergence and in the plot files are stored column-wise.
Please note that all physical quantities are defined in SI units.

In order to run "Unstruct2D", a grid file must be provided. The grid file
contains the dimensions of the grid, the description of the boundary faces,
the Cartesian coordinates of the nodes and the nodes of the triangles. The
format of the grid file is as follows:

(1) dimensions (one line):
    - number of grid nodes (NO dummy nodes)
    - number of triangular cells,
    - number of boundary conditions

(2) for each boundary condition (two lines):
    - type
      100-199 = inflow
      200-299 = outflow
      300-399 = viscous wall
      400-499 = inviscid wall
      500-599 = symmetry line
      600-699 = farfield
      700-799 = periodic boundary
    - index of the last boundary face (NOT used for periodic boundaries)
    - index of the last boundary point
    - name (on new line)

(3) list of boundary faces (2 nodes of a face, arbitrary oriented) for all
    boundary conditions except if the corresponding boundary is periodic,
    then the list contains the two related periodic nodes instead

(4) Cartesian coordinates of the nodes (first column is x, second is y)

(5) indexes of three nodes defining a triangle; the orientation is arbitrary.

Only ONE of the two related periodic boundaries may be included in the
boundary conditions (point 2 above). The nodes of the "shadow" periodic
boundary are obtained from the list of boundary nodes (point 3). Note that
all indexing in the grid file starts from 1, this is corrected when reading
the data (the aim is compatibility with the Fortran example). The program
contained in "grid_unstr_2d" provides a good example of how a grid file can
be prepared.
