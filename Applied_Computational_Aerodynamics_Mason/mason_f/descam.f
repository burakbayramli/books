c
c     REM  CAMBER LINE DESIGN
c
c     two dimensional linear theory 
c         for incompressible flow

c     based on Lan's Quasi Vortex Lattice method

c     W.H. Mason, 
c     last mod: March 2, 2004 by Leifur Thor Leifsson

c     TI-59 calculator program - 1981
c     Apple ][ BASIC           - 1983
c     Macintosh QuickBASIC     - 1991
c     Macintosh FORTRAN        - 1991
c     Compaq Visual Fortran    - 2001

C     Mods by Andy Ko 1/16/01
c       - Added output to file.
c       - Added Pause statement at end to prevent window 
c         from closing after program ends.
c       - Added header statement to output.
c	Mods by Leifur Thor Leifsson 03-02-04
c	  - Changed header statement
      
      DIMENSION XQ(101),DC(101),Z(121),ZB(121),DZ(121)
      character*20 filenm, outputfile
      character*60 case
      pi = 3.141592654

c      write(6,35)
	write(6,3000)
	write(6,3001)
	write(6,3002)
	write(6,3003)

      write(6,40)
      read(5,45) filenm
      open(unit = 2, file = filenm, status = 'old')
C     Added output file
      write(6,41)
	read(5,45) outputfile
      open(unit = 3, file = outputfile, status = 'replace')

c      write(3,35)
	write(3,3000)
	write(3,3001)
	write(3,3002)
	write(3,3003)
	write(3,3004)

      read(2,50) case

      read(2,55) fnq
      nq  = fnq

      write(6,60) case
	write(3,60) case
      write(6,70)
	write(3,70)
      do 5 n = 1,nq
      read(2,55) xq(n),dc(n)
      write(6,80) n,xq(n),dc(n)
    5 write(3,80) n,xq(n),dc(n)
c
c     compute CL and Cm of input chord load
c
      S1 = 0
      S2 = 0

      do 10 I = 2,nq
      DQ = (XQ(I) - XQ(I - 1)) / 2.
      S1 = S1 + DQ * (DC(I) + DC(I - 1))
C	Bug report by Thomas Zeiler:
C	S2 is wrong: Cm doesn't match Theory of Wing Sections results
C		- The sign is wrong
C		- The centroid of the uniform-plus-triangular pressure
C		  distribution area isn't at the center of the Delta-x.
c      S2 = S2 + DQ * ((XQ(I) - .25) * DC(I) +
c     1                (XQ(I - 1) - .25) * DC(I - 1))
c	Suggested code:
	S2 = S2 - DQ*((XQ(I) - .25)*DC(I)
     1		-(2.e0*DQ/3.e0)*(DC(I) - DC(I-1))
	2		+(XQ(I-1) - .25)*DC(I-1))
   10 continue
      
      write(6,110) s1,s2
	write(3,110) s1,s2

      write(6,120)
      read(5,*) NM

      write(3,121) NM
      xnm           = nm

c     compute camber line

c     compute slope distribution first

      ncount       = nm + 1
      do 20 icount = 1,ncount
      i            = icount - 1
      index        = i + 1
      XI           = (1. -  COS (I*pi/xnm))/2.
      S1           = 0.
      J            = 1
      do 15      K = 1,NM
      XK           = (1. -  COS ((2 * K - 1) * pi/2./xnm))/2.
      call interp(xk,ck,xq,dc,nq)
      S1           = S1 + CK/4. * sqrt(XK * (1. - XK))/(XI - XK)
   15 continue
      DZ(index)    =  - S1/xnm
   20 continue

c     integrate slopes to get camber line and angle of attack

      XP           = 1.0
      Z(NM+1)      = 0.0
      do 25 icount = 1,nm
      i            = nm - icount
      index        = i + 1
      XI           = (1. -  COS (I * pi/xnm))/2.
      Z(index)     = Z(index+1) - (XP - XI) * 
     1                            (DZ(index) + DZ(index+1))/2.
      XP           = XI
   25 continue 

      ALPDES       = 180.0/pi*ATAN(Z(1))

      write(6,130) ALPDES
	write(3,130) ALPDES

c     write out result and remove angle of attack to get standard
c     camber line definition (LE and TE = 0)

      write(6,135)
	write(3,135)

      ncount       = nm + 1
      do 30 icount = 1,ncount
      i            = icount - 1
      XI           = (1. -  COS (I * pi/xnm)) / 2.
      ZB(icount)   = Z(icount) - (1. - XI) * Z(1)
      write(6,140) I,XI,Z(icount),DZ(icount),ZB(icount)
	write(3,140) I,XI,Z(icount),DZ(icount),ZB(icount)
   30 continue

      Pause 'Press the ENTER key to close program...'

      stop

   35 format(/5x,'*************************************************'/
     1        5x,'Program descam            version from W.H. Mason'/
     2        5x,'The Department of Ocean & Aerospace Engineering,'/
     3        5x,'Virginia Tech,'/ 
     4        5x,'Blacksburg, VA 24060.'/
     5        5x,'http://www.aoe.vt.edu'//
     6        5x,'Camber line design program'/
     7        5x,'*************************************************'/)
   40 format(/1x,'enter name of input data file')
   41 format(/1x,'enter name of output data file')
   45 format(a20)
   50 format(a60)
   55 format(2f10.5)
   60 format(
     1 //'  camber line design using quasi-vortex lattice method',
     2 //'  case title:  ',a60/)
   70 format(/3x,'design chord load'//4x,'n',10x,
     1       'x/c',9x,'Delta Cp')
   80 format(i5,f15.5,f14.4)
  110 format(/8x,'CL = ',f8.4,5x,'Cm = ',f8.4/)
  120 format(' enter the number of points to be computed:'/)
  121 format(' Number of points that was computed = ',i5 /)
  130 format(/3x,'Design angle of attack = ',f8.4,' degrees'/)
  135 format(4x,'i',10x,'x/c',12x,'z/c',11x,'dz/dx',8x,'(z-z0)/c')
  140 format(i5,3f15.5,f15.6)
3000  FORMAT('  **************************************************'/
     1        '    VT Aerospace Aircraft Design Software Series'/
     2		'  **************************************************'/)
3001  FORMAT('                   Program descam'/
     1		/'  Camber line design - Two dimensional linear theory'/
     2		 '  for incompressible flow.'/
     3		/'  Based on Lans Quasi Vortex Lattice Method.'/) 
3002  FORMAT('  Code written by William H. Mason.'/
     1			'  A bug reported by Thomas Zeiler, September, 2001.'/
     1			'  Modified by'/
     2			'    - Andy Ko, January, 2001.'/
     3			'    - Leifur Thor Leifsson, March, 2004.'/)
3003  FORMAT('  The Department of Aerospace and Ocean Engineering,'/
     1		'  Virginia Tech, Blacksburg, VA 24061.'/
     2		'  http://www.aoe.vt.edu, email: whmason@vt.edu')
3004  FORMAT(/'  **************************************************'/)

      END

      subroutine interp(xk,ck,xq,dc,nq)
      
c     REM  LINEAR INTERPOLATION

      dimension xq(1),dc(1)
      j  = nq/2
      DX = XK - XQ(J)
      IF (DX .gt. 0.0) go to 7020
      IF (DX .eq. 0.0) go to 7030
7010  IF (J  .eq. 1)   go to 7030
      J  = J - 1
      DX = XK - XQ(J)
      IF (DX .lt. 0.0) go to 7010
      IF (DX .le. 0.0) go to 7030
7019  J  = J + 1
      DX = DY
7020  IF (J .eq. NQ) go to 7030
      DY = XK - XQ(J + 1)
      IF (DY .ge. 0.0) go to 7019
7030  CK = DC(J) + (DC(J + 1) - DC(J))/
     1             (XQ(J + 1) - XQ(J))*(XK - XQ(J))

      return
      end 
