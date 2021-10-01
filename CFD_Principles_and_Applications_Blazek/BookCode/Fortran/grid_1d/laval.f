      PROGRAM Laval

C *****************************************************************************
C
C   GENERATES GRID & COMPUTES AREA FOR LAVAL NOZZLE
C   ===============================================
C
C   (c) Jiri Blazek, CFD Consulting & Analysis, www.cfd-ca.de
C   Created June 18, 1996
C   Version 1.2 from Dec. 22, 2004
C
C
C   This program is free software; you can redistribute it and/or
C   modify it under the terms of the GNU General Public License
C   as published by the Free Software Foundation; either version 2
C   of the License, or (at your option) any later version.
C
C   This program is distributed in the hope that it will be useful,
C   but WITHOUT ANY WARRANTY; without even the implied warranty of
C   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C   GNU General Public License for more details.
C
C   You should have received a copy of the GNU General Public License
C   along with this program; if not, write to the Free Software
C   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************

      IMPLICIT NONE

C ... parameters - set as appropriate
      INTEGER idims
      PARAMETER (idims=500)

C ... local variables
      CHARACTER*256 fngrid
      INTEGER ncells, ncthroat, imax, ib2, i
      REAL*8  x(idims), a(idims)
      REAL*8  a1, a2, dx, xthroat, ang, pi

C *****************************************************************************

      pi = 4.D0*ATAN(1.D0)

C --- read user input

      WRITE(*,1000)

      READ(*,'(A256)') fngrid
      READ(*,*) a1
      READ(*,*) a2
      READ(*,*) ncells
      READ(*,*) ncthroat

C --- generate grid (0 <= x <= 1) - equal spacing
C     total nozzle length = 1.0

      ib2  = ncells + 2
      imax = ncells + 3
      IF (imax .GT. idims) THEN
        WRITE(*,1005) imax,idims
        STOP
      ENDIF

      WRITE(*,'(A)') 'Distributing grid nodes ...'

      dx   = 1.D0/DBLE(ncells)
      x(1) = 0.D0
      x(2) = 0.D0
      DO i=3,ib2
        x(i) = x(i-1) + dx
      ENDDO
      x(ib2 ) = 1.D0
      x(imax) = x(ib2)

      xthroat = dx*DBLE(ncthroat)

C --- calculate area

      WRITE(*,'(A)') 'Calculating cross sections ...'

      a(1) = a1
      a(2) = a1
      DO i=3,ib2-1
        IF (x(i) .LE. xthroat) THEN
          ang  = x(i)*pi/xthroat
          a(i) = 1.D0 + 0.5D0*(a1-1.D0)*(1.D0+COS(ang))
        ELSE
          ang  = (x(i)-xthroat)*pi/(1.D0-xthroat)
          a(i) = 1.D0 + 0.5D0*(a2-1.D0)*(1.D0-COS(ang))
        ENDIF
      ENDDO
      a(ib2 ) = a2
      a(imax) = a2

C --- store the grid file

      WRITE(*,'(A)') 'Storing grid file ...'

      OPEN(10,file=fngrid,form='formatted',status='unknown')
      WRITE(10,1010) imax
      WRITE(10,1015) (x(i),a(i), i=1,imax)
      CLOSE(10)

      WRITE(*,'(A,/)') 'Finished.'

1000  FORMAT(//,' ***************************************************',
     &       //,'         GENERATION OF 1-D GRID IN A NOZZLE',//,
     &       '    (c) J. Blazek, Version 1.2 from Dec. 22, 2004',//,
     &       ' ***************************************************',//)
1005  FORMAT(' ERROR - too many grid points! (',I3,' but max. ',
     &       I3,')')
1010  FORMAT(I5)
1015  FORMAT(2E16.8)
      STOP
      END
