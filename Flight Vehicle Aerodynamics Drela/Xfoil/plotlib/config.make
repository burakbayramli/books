
#=======================================#
# Makefile options for Xplot11 library  #
#   Set up or select a set of compile   #
#   options for your system             # 
#=======================================#


### Use these to set library name 
### (you might add DP to name to keep double precision version separate)
PLTLIB = libPlt.a
#PLTLIB = libPltDP.a


# Some fortrans need trailing underscores in C interface symbols (see Xwin.c)
# This should work for most of the "unix" fortran compilers
DEFINE = -DUNDERSCORE


###-------------------------------------------------------------------------
###  Uncomment for Linux, using the script fort77 or yaf77 or old f77 script
###  Compiler options for Linux GNU compilers include:
###   fort77 perl script (calls f2c/gcc) from RH or from yaf77
###          or the yaf77 or the old f77 shell script from f2c
###   g77    the GNU Fortran compiler 
#
#FC = g77-3
#FC = fort77
#CC  = gcc
# Uncomment DP to make double-precision version
# (note -r8 does not work in g77, use f2c instead)
#DP = -r8
#FFLAGS  = -O2 $(DP)
#CFLAGS  = -O2 $(DEFINE)
#AR = ar r
#RANLIB = ranlib 
#LINKLIB = -L/usr/X11R6/lib -lX11 

###-------------------------------------------------------------------------
###  Uncomment for Linux, using PGI f77
#FC = pgf77
#CC  = gcc
##
# Uncomment to make double-precision version
#DP = -r8
#FFLAGS  = -fast -O $(DP)
#CFLAGS  = -O2 $(DEFINE)
#AR = ar r
#RANLIB = ranlib 
#LINKLIB = -L/usr/X11R6/lib -lX11

###-------------------------------------------------------------------------
###  Uncomment for Linux, using Intel Fortran compiler 8.x
FC = ifort
CC  = gcc
 
# Uncomment to make double-precision version
#DP = -r8

FFLAGS  = -O3 $(DP)
CFLAGS  = -O3 $(DEFINE)
AR = ar r
RANLIB = ranlib 

LINKLIB = -L/usr/X11R6/lib -lX11


###-------------------------------------------------------------------------
###  Uncomment for DEC OSF/Alpha
#FC = f77 
#
# Uncomment DP to make double-precision version
#DP = -r8
#CFLAGS = -O4 -float $(DEFINE)
#FFLAGS = -O4 $(DP)
# Debug flags
#CFLAGS = -O0 -g -float $(DEFINE) 
#FFLAGS = -O0 -g $(DP)
#LINKLIB =  -lX11

###-------------------------------------------------------------------------
###  Uncomment for RS/6000
# Note if the library is compiled double precision use the -qautodbl=dbl4 
# option, not the -qautodbl=dblpad4 option.  The dblpad4 option puts padding
# into the argument lists for integer args that cause the polylines and 
# linepatterns to fail as the alignment assumptions between the C and fortran
# routines are then different.  (The problem lies with xlf90, at least you 
# can cure it with a compile option:-). This is not a problem on xlf (f77) 
# because it doesn't have a dblpad4 option...
#
#FC = xlf90
#
# Uncomment DP to make double-precision version
#DP = -qautodbl=dbl4
#FFLAGS = -O -qextname -qfixed $(DP) 
#### Link libs required for xlf90 at ABB  (HHY 9/96)
#LINKLIB =  -lX11   -L/venus/u1/fortran/libfor -lxlfabb 

###-------------------------------------------------------------------------
###  Uncomment for Sun Open-Windows 
###  (give location of X11/xxx.h include files)
#
# Uncomment DP to make double-precision version
#DP = -r8
#FFLAGS = -O $(DP)
#CFLAGS = -O -I/usr/openwin/share/include $(DEFINE)
#LINKLIB =  -lX11

###-------------------------------------------------------------------------
###  Uncomment for HP-9000
###  (use ANSI-C standard, use underscored C-routine names)
#
# Uncomment DP to make double-precision version
#DP = -r8
#CFLAGS = -O -Aa $(DEFINE)
#FFLAGS = -O +ppu $(DP)
#OBJMISC = util-ops.o
#LINKLIB =  -lX11

###-------------------------------------------------------------------------
###  Uncomment for SGI IRIX
###  (use ANSI-C standard, use underscored C-routine names)
#
# Uncomment DP to make double-precision version
#DP = -r8
#CFLAGS = -O $(DEFINE)
#FFLAGS = -O -static  $(DP)
#RANLIB = ar qs
#LINKLIB =  -lX11

