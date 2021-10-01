********************************************************************************

  S T R U C T 2 D

  (c) 1997-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for the solution of 2-D Euler and Navier-Stokes equations on
structured single-block grids using a finite-volume scheme. Spatial
discretization is conducted by the cell-centered scheme with either central
or upwind dissipation. The equations are integrated in time using an
explicit multistage scheme. The program also offers the possibility to
simulate incompressible flows at very low Mach numbers. Further details are
provided in Section 12.6 of the book.


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

channel - inviscid, transonic flow in a channel with 10% circular bump
fplate  - laminar viscous flow past a flat plate
n0012   - inviscid, transonic flow around NACA 0012 airfoil
rg15a   - inviscid, incompressible flow around RG 15A-1.8/11.0 airfoil
srm     - flow inside a test solid rocket motor (Traineau, J.-C.; Hervat,
          P.; Kuentzmann, P.: "Cold-Flow Simulation of a Two-Dimensional
          Nozzleless Solid Rocket Motor". AIAA Paper 86-1447, 1986)
vki1    - inviscid, transonic flow inside VKI-1 turbine cascade (Kiock, R.;
          Lehthaus, F.; Baines, N.C.; Sieverding, C.H.: "The Transonic Flow
          Through a Plane Turbine Cascade as Measured in Four European Wind
          Tunnels". ASME J. Engineering Gas Turbines and Power, 108 (1986),
          pp. 277-284)

Files with the extension "grd" are grid files, with "top" the related topology
files. Files with "_input" added to the case name represent user input files.
Letter "c" at the end of a name means that the setup is for, or the result is
computed by, the central scheme with scalar artificial dissipation (Subsection
4.3.1). On the other hand, letter "r" denotes Roe's upwind scheme (Subsection
4.3.3). Furthermore, "iso" in the name signifies a plot file containing the
complete 2-D flow-field, whereas "surf" stands for the surface only. The
directory "run" contains in "Struct2D.exe" the executable program for 64-bit
Windows platforms (Win 7 and higher).


--------------------------------------------------------------------------------
  How to compile "Struct2D"
--------------------------------------------------------------------------------

For the compilation of the sources in "src", any decent Fortran-90 compiler
is sufficient. If you have none installed, GNU's freely available gfortran
compiler is highly recommendable for the purpose. On Windows, the compiler
is provided in the MinGW package. An easy to install distribution can be
downloaded at: www.tdm-gcc.tdragon.net/download. It offers the possibility
to compile the sources to either 32- or 64-bit executable.

The directory "src" contains also project file for the Code::Blocks
(www.codeblocks.org) integrated development environment, which has a very
nice plug-in for Fortran-90 projects. The IDE is available for free for all
major platforms. But of course, you can use your favorite IDE or just an
editor as well. For this purpose, a Makefile is provided in "src". The file
"main.f90" contains the main program, module files are named "mod*.f90".


--------------------------------------------------------------------------------
  How to run "Struct2D"
--------------------------------------------------------------------------------

The program requires a number of user input parameters which determine the
fluid properties, set the boundary conditions, control the program
execution, and the output of the results. The user parameters are stored in
a plain ASCII file. Examples are provided in the files "*_input_*" in the
directory "run". In order to run "Struct2D", open a terminal window (Command
Prompt on Windows), change to the directory where the grid and user input
files are located and type at the prompt:

> Struct2D input_file

where "input_file" is the path and name of the file with user parameters
(e.g., "channel_input_r"). Depending on how your environment is set up (PATH
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
i-res  - i-index of the grid node where resmax is found
j-res  - j-index of the grid node where resmax is found
cl     - lift coefficient   }
cd     - drag coefficient   } accounting for pressure forces only!
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

In order to run "Struct2D", a grid and topology file must be provided. The
grid file contains the dimensions of the grid and the Cartesian coordinates
of the nodes. The coordinates are stored column-wise (x, y). They are
ordered with the inner loop running over the first index (i-coordinate in
the computational space - see Fig. 12.4). Thus, the coordinates can be read
in by the following statement:

READ(iunit,*) ((x(i,j),y(i,j), i=2,il), j=2,jl)

The topology file describes the division of the boundaries into segments and the
type of the boundary condition for each segment. It also contains information
about the connections between cut or periodic boundaries. The description of a
segment consists of two lines. The first line contains the name of the boundary
condition (not used within the program). The second line contains seven integer
values:

itype - boundary type:
        100-199 = inflow
        200-299 = outflow
        300-399 = viscous wall
        400-499 = inviscid wall
        500-599 = symmetry line
        600-699 = farfield
        700-799 = coordinate cut or periodic boundary
        800-899 = mass injection
lb    - side of the computational domain (1: j=2, 2: i=i2, 3: j=j2, 4: i=2;
        see Fig. 12.4)
lbeg  - start index of the segment (given as cell index, NOT node index)
lend  - end index of the segment
lbs   - side of the computational domain where the source (partner) segment is
        located (if 700<=itype<=799)
lbegs - start index of the source segment
lends - end index of the source segment

It should be noted that the start index of a segment can be greater than the
end index (see, e.g., "n0012.top"). The program reads the above values into an
array lbsegs (subroutine ReadTopology in "readTopology.f90").
