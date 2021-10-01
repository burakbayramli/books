      SUBROUTINE Rgrid( idim,mgdim,nlevels,imax,ib2,ncells,x,a )

C *****************************************************************************
C
C  Reads in the grid (x-coordinate and nozzle area);
C  checks its dimensions against "idim";
C  generate coarse grids.
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: Get_addr
C
C *****************************************************************************
C
C  Created    : Jul. 25, 1997; (c) Jiri Blazek
C  Last update: Dec. 28, 2004
C
C  This program is free software; you can redistribute it and/or
C  modify it under the terms of the GNU General Public License
C  as published by the Free Software Foundation; either version 2
C  of the License, or (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public License
C  along with this program; if not, write to the Free Software
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C *****************************************************************************

      IMPLICIT NONE
      INCLUDE 'param.inc'

C ... parameter list
      INTEGER idim, mgdim, nlevels
      INTEGER imax(nlevels), ib2(nlevels), ncells(nlevels)
      REAL*8  x(mgdim*idim), a(mgdim*idim)

C ... local variables
      INTEGER i, ilev, iaddrf, iaddrc

C ... functions
      INTEGER Get_addr

C *****************************************************************************

      OPEN(10,file=fngrid,form='formatted',status='old')
      READ(10,*) imax(1)
      IF (imax(1) .GT. idim) THEN
        WRITE(*,1000) imax(1),idim
        STOP
      ENDIF
      READ(10,*) (x(i),a(i), i=1,imax(1))
      CLOSE(10)

C --- coarse grids

      ncells(1) = imax(1) - 3
      ib2(1)    = imax(1) - 1
      DO ilev=2,nlevels
        ncells(ilev) = ncells(ilev-1)/2
        IF (2*ncells(ilev) .NE. ncells(ilev-1)) THEN
          WRITE(*,1005) ilev-1
          STOP
        ENDIF
        imax(ilev) = ncells(ilev) + 3
        ib2(ilev)  = ncells(ilev) + 2
        iaddrf     = Get_addr( nlevels,ilev-1,1,imax ) - 1
        iaddrc     = Get_addr( nlevels,ilev  ,1,imax ) - 1
        IF (iaddrc+imax(ilev) .GT. mgdim*idim) THEN
          WRITE(*,1010) ilev,iaddrc+imax(ilev),mgdim*idim
          STOP
        ENDIF
        DO i=2,ib2(ilev)
          x(iaddrc+i) = x(iaddrf+2*i-2)
          a(iaddrc+i) = a(iaddrf+2*i-2)
        ENDDO
        x(iaddrc+1)          = x(iaddrf+1)
        a(iaddrc+1)          = a(iaddrf+1)
        x(iaddrc+imax(ilev)) = x(iaddrf+imax(ilev-1))
        a(iaddrc+imax(ilev)) = a(iaddrf+imax(ilev-1))
      ENDDO

1000  FORMAT(/,' Error (rgrid.f) - too many grid points! (',I3,
     &       ' but max. ',I3,')')
1005  FORMAT(/,' Error (rgrid.f) - no. of cells on level ',
     &       I1,' not divisible by 2!')
1010  FORMAT(/,' Error (rgrid.f) - insufficient memory for level ',
     &       I1,'! (',I3,' but max. ',I3,')')
      RETURN
      END
