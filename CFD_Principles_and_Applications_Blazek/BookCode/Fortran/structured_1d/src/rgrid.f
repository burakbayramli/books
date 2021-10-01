      SUBROUTINE Rgrid( idim,imax,ib2,x,a )

C *****************************************************************************
C
C  Reads in the grid (x-coordinate and nozzle area);
C  checks its dimensions against "idim".
C
C *****************************************************************************
C
C  Subroutines called: none
C
C  Functions called: none
C
C *****************************************************************************
C
C  Created    : Jul. 25, 1997; (c) Jiri Blazek
C  Last update: Feb. 28, 1999
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
      INTEGER idim, imax, ib2
      REAL*8  x(idim), a(idim)

C ... local variables
      INTEGER i

C *****************************************************************************

      OPEN(10,file=fngrid,form='formatted',status='old')
      READ(10,*) imax
      IF (imax .GT. idim) THEN
        WRITE(*,1000) imax,idim
        STOP
      ENDIF
      READ(10,*) (x(i),a(i), i=1,imax)
      CLOSE(10)

C --- last physical point & no. of cells

      ib2    = imax - 1
      ncells = imax - 3

1000  FORMAT(/,' Error (rgrid.f) - too many grid points! (',I3,
     &       ' but max. ',I3,')')
      RETURN
      END
