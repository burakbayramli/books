********************************************************************************

  GRID GENERATION LIBRARY

  (c) 2000-2014 Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de

********************************************************************************

Collection of subroutines needed for the 2-D grid generation codes "cgrid",
"channel" and "hgrid". The subroutines are compiled and linked into a library
named "libgrid.a".


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

Makefile   - compilation options
README.txt - what you read now
bezier.f   - Bezier spline interpolation
ellgrid.f  - elliptic grid generation (Laplace & Poisson equations)
length.f   - returns length of a string without trailing blanks
sstretch.f - S-type stretching function
stretch.f  - stretching function
tfint.f    - linear transfinite interpolation


--------------------------------------------------------------------------------
  How to compile "libgrid.a"
--------------------------------------------------------------------------------

Edit first the flags (SRC, EXEC, O, LD, FC, FFLAGS, and LDFLAGS) in "Makefile"
as appropriate for your computer, compiler and/or operating system. There are
examples of settings for GNU g77, Mac OS X, IBM, SGI and SUN f77 compilers. Type
"make" at the prompt to compile the subroutines and to create the library. All
object files and the library can be deleted by "make clean".
