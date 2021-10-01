********************************************************************************

  M S T A G E

  (c) 1992-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Program for von Neumann stability analysis of explicit multi-stage schemes
using linear 1-D model equation of convection or mixed convection-diffusion
type. See Chapter 10.3 for the mathematical details.


--------------------------------------------------------------------------------
  Files
--------------------------------------------------------------------------------

Makefile    - compilation options
README.txt  - what you read now
ampl_c.v2d  - plot of amplification factor generated with "input_c"
ampl_u2.v2d - plot of amplification factor generated with "input_u2"
ampl_u.v2d  - plot of amplification factor generated with "input_u"
ampl_v.v2d  - plot of amplification factor generated with "input_v"
central.pdf - PDF file with results (corresponding to "input_c")
input_c     - example input file for central spatial discretization
              with central implicit residual smoothing (CIRS)
input_u     - example input file for upwind spatial discretization
              with CIRS
input_u2    - example input file for upwind spatial discretization
              with upwind implicit residual smoothing
input_v     - example input file for convection-diffusion model equation,
              upwind spatial discretization and CIRS
mstage.exe  - executable for Windows 64-bit platform
mstage.f    - the analysis program
stab_c.v2d  - plot of Fourier symbols and stability region generated
              with "input_c"
stab_u2.v2d - plot of Fourier symbols and stability region generated
              with "input_u2"; resolution of the Fourier symbol with
              upwind residual smoothing increased to 14401 points
stab_u.v2d  - plot of Fourier symbols and stability region generated
              with "input_u"
stab_v.v2d  - plot of Fourier symbols and stability region generated
              with "input_v"
upwind2.pdf - PDF file with results (corresponding to "input_u2")
upwind.pdf  - PDF file with results (corresponding to "input_u")
viscous.pdf - PDF file with results (corresponding to "input_v")


--------------------------------------------------------------------------------
  How to compile "mstage"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, RM, LD, FC, FFLAGS, and LDFLAGS) in
"Makefile" as appropriate for your computer, compiler and/or operating system.
There are examples of settings for GNU gfortran, Mac OS X, IBM, SGI and SUN f77
compilers. Type "make" at the prompt to compile the program. The object file
and the executable can be deleted by "make clean".


--------------------------------------------------------------------------------
  How to run "mstage"
--------------------------------------------------------------------------------

The program reads a number of user parameters from the standard input. The user
parameters are stored in a plain ASCII file. Examples are provided in the files
"input_*". In order to run "mstage", you have to type at the prompt:

% mstage < [input file]

where [input file] is the path and name of the file with parameters, e.g.:

% mstage < input_c

The program generates two plot files (in the file "input_c" named as
"stab_c.v2d" and "ampl_c.v2d"). The first plot file consists of three regions:

(1) 100x100 grid with values of |g| (for contour plot); variables: x, y, |g|
(2) locus of the Fourier symbol without smoothing; variables: x, y
(3) locus of the Fourier symbol with residual smoothing; variables: x, y

The second plot file consists of two regions:

(1) |g| as the function of the phase angle; variables: angle, |g|
(2) same as above but with residual smoothing switched on

The data in each plot file are stored in Vis2D format and can be visualized
using the package "Vis2D" available at www.cfd-ca.de.


--------------------------------------------------------------------------------
  Input parameters
--------------------------------------------------------------------------------

The program requires the specification of the following parameters:

fnstab     = plot file with stability region and Fourier symbols
             (ASCII Vis2D format)
fnamplif   = plot file with amplification factors (ASCII Vis2D format)
modeq      = model equation (1=convection, 2=convection+diffusion)
kdis       = spatial discretization (0=central, 1=1st-order upwind,
             2=2nd-order upwind)
ksmoo      = type of implicit residual smoothing (1=central, 2=upwind)
ksch       = multi-stage scheme (e.g., 53 = (5,3)-scheme)
cflu       = CFL-number basic scheme
cfls       = CFL-number with residual smoothing
ratio      = ratio of viscous to convective eigenvalue (0=convection only)
eps4       = artificial dissipation coefficient (kdis=0 only)
esmoo      = smoothing coefficient
a()        = stage coefficients
xbeg, xend = range of the x-axis for the plot of stability region
ybeg, yend = range of the y-axis

Further details are provided in the example files "input_*".


--------------------------------------------------------------------------------
  Main variables
--------------------------------------------------------------------------------

The following are the most important variables:

dtrat = ratio of convective to viscous time step (= |lam|/(|lam|+C*lam_v)
f     = Fourier symbol of the time-stepping operator
s     = Fourier symbol of the smoothing operator
z     = Fourier symbol of the spatial operator
ze    = Fourier symbol of the spatial operator after residual smoothing
zi    = imaginary part of z
zr    = real part of z
zrv   = contribution of the diffusion term to z

Variables f, s, and z are declared as COMPLEX, the other variables as REAL.
Switches and loop indexes are declared as INTEGER.


--------------------------------------------------------------------------------
  Program structure
--------------------------------------------------------------------------------

Tree of subroutine calls:

MSTAGE - main program
  AMPLIF  - Fourier symbol of the time-stepping operator; amplification factor
  DIFFOP  - Fourier symbol of the convection term
  DIFFOPV - Fourier symbol of the diffusion term
  IRSOP   - Fourier symbol of the residual smoothing operator

The main program and the subroutines are all in the same file "mstage.f".
There are no include files and common blocks.
