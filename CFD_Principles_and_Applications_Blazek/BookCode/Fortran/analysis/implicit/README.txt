********************************************************************************

  I M P L I C

  (c) 1992-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for von Neumann stability analysis of a general implicit time-stepping
scheme using linear 1-D model equation of convection or mixed convection-
diffusion type. See Chapter 10.3 for the mathematical details.


--------------------------------------------------------------------------------
  Files
--------------------------------------------------------------------------------

Makefile   - compilation options
README.txt - what you read now
amplif.pdf - PDF file with results (generated with "input12" and "input22")
implic.exe - executable for Windows 64-bit platform
implic.f   - the analysis program
input12    - example input file with 1st-order upwind on the LHS
             and 2nd-order upwind on the RHS
input22    - example input file with 2nd-order upwind on both sides
plot12.v2d - plot of amplification factor generated with "input12"
plot22.v2d - plot of amplification factor generated with "input22"


--------------------------------------------------------------------------------
  How to compile "implic"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, RM, LD, FC, FFLAGS, and LDFLAGS) in
"Makefile" as appropriate for your computer, compiler and/or operating system.
There are examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77
compilers. Type "make" at the prompt to compile the program. The object file
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "implic"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. Examples are provided in the files
"input*". In order to run "implic", you have to type at the prompt:

% implic < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% implic < input12

The program generates a plot file (in the file "input12" named as
"plot12.v2d"). The plot file contains two variables - the phase angle (first
column) and the magnitude of the amplification factor (second column). The
data is stored in Vis2D format and can be displayed using the program
"Vis2D" available at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The program requires the specification of the following parameters:

fnplot    = file with plot data (ASCII Vis2D format)
mod	      = model equation (1=convection, 2=convection+diffusion)
kdis_expl = spatial discretization of the explicit operator (RHS):
            0=central+4th differences, 1=1st-order upwind,
            2=2nd-order upwind
eps_e4    = coefficient of artificial dissipation (RHS) if kdis_expl=0
kdis_impl = spatial discretization of the implicit operator (LHS):
            0=central+2nd differences, 1=1st-order upwind,
            2=2nd-order upwind
eps_i2    = coefficient of artificial dissipation (LHS) if kdis_impl=0
beta      = parameter multiplying the flux Jacobian - controls the time
            accuracy (1.0=1st-order in time, 0.5=2nd-order)
cfl	      = CFL-number
ratio	    = ratio of viscous to convective eigenvalue (0=convection only)
diffterm  = diffusion term included in the implicit operator (0=no, 1=yes)

Further details are provided in the example files "input*".


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

af     = magnitude of the amplification factor
g      = amplification factor
z_expl = Fourier symbol of the explicit operator
z_impl = Fourier symbol of the implicit operator
zi     = imaginary part of z
zr     = real part of z
zrve   = contribution of the diffusion term to z_expl
zrvi   = contribution of the diffusion term to z_impl (if diffterm>0)

Variables g, z_expl, and z_impl are declared as COMPLEX, the other variables
as REAL. Switches and loop indexes are declared as INTEGER.


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

The program consists only of the main routine. There are no include files and
common blocks.
